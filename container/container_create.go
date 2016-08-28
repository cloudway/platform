package container

import (
	"archive/tar"
	"bytes"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"io/ioutil"
	"net/http"
	"os"
	"path/filepath"
	"strconv"
	"strings"
	"text/template"

	"github.com/Sirupsen/logrus"
	"github.com/docker/engine-api/types"
	"github.com/docker/engine-api/types/container"
	"github.com/docker/engine-api/types/network"
	"github.com/docker/engine-api/types/strslice"
	"golang.org/x/net/context"

	apitypes "github.com/cloudway/platform/api/types"
	"github.com/cloudway/platform/config"
	"github.com/cloudway/platform/config/defaults"
	"github.com/cloudway/platform/pkg/archive"
	"github.com/cloudway/platform/pkg/files"
	"github.com/cloudway/platform/pkg/manifest"
	. "github.com/cloudway/platform/scm"
)

type CreateOptions struct {
	Name        string
	Namespace   string
	ServiceName string
	Plugin      *manifest.Plugin
	Secret      string
	Home        string
	User        string
	Network     string
	Capacity    string
	Scaling     int
	Hosts       []string
	Env         map[string]string
	Repo        string
	Log         io.Writer
}

type createConfig struct {
	*CreateOptions
	Env               map[string]string // duplicate env map to prevent destruct original map
	PluginInstallPath string
	Category          manifest.Category
	BaseImage         string
	InstallScript     string
	DependsOn         []string
	Hostname          string
	FQDN              string
	Debug             bool
}

// Create new application containers.
func (cli DockerClient) Create(ctx context.Context, scm SCM, opts CreateOptions) ([]*Container, error) {
	cfg := &createConfig{CreateOptions: &opts}
	cfg.Env = make(map[string]string)
	for k, v := range opts.Env {
		cfg.Env[k] = v
	}

	meta := opts.Plugin

	if cfg.User == "" {
		if meta.User != "" {
			cfg.User = meta.User
		} else {
			cfg.User = defaults.AppUser()
		}
	}
	if cfg.Home == "" {
		cfg.Home = defaults.AppHome()
	}
	if cfg.Network == "" {
		cfg.Network = config.Get("network")
	}

	cfg.Category = meta.Category
	cfg.PluginInstallPath = meta.Name + "-" + meta.Version
	cfg.BaseImage = meta.BaseImage
	cfg.DependsOn = meta.DependsOn
	cfg.Debug = config.Debug

	cfg.Env["CLOUDWAY_APP_NAME"] = cfg.Name
	cfg.Env["CLOUDWAY_APP_NAMESPACE"] = cfg.Namespace
	cfg.Env["CLOUDWAY_SHARED_SECRET"] = cfg.Secret
	cfg.Env["CLOUDWAY_APP_USER"] = cfg.User
	cfg.Env["CLOUDWAY_HOME_DIR"] = cfg.Home
	cfg.Env["CLOUDWAY_REPO_DIR"] = cfg.Home + "/repo"
	cfg.Env["CLOUDWAY_DATA_DIR"] = cfg.Home + "/data"
	cfg.Env["CLOUDWAY_LOG_DIR"] = cfg.Home + "/logs"

	switch cfg.Category {
	case manifest.Framework:
		return createApplicationContainer(cli, ctx, scm, cfg)
	case manifest.Service:
		return createServiceContainer(cli, ctx, cfg)
	default:
		return nil, fmt.Errorf("%s:%s is not a valid plugin", meta.Name, meta.Version)
	}
}

func createApplicationContainer(cli DockerClient, ctx context.Context, scm SCM, cfg *createConfig) ([]*Container, error) {
	if cfg.ServiceName != "" {
		return nil, fmt.Errorf("The application name cannot contains a serivce name: %s", cfg.ServiceName)
	}

	cfg.Hostname = cfg.Name + "-" + cfg.Namespace
	cfg.FQDN = cfg.Hostname + "." + defaults.Domain()
	cfg.Env["CLOUDWAY_APP_DNS"] = cfg.FQDN

	scale, err := getScaling(cli, ctx, cfg.Name, cfg.Namespace, cfg.Scaling)
	if err != nil {
		return nil, err
	}

	image, err := buildImage(cli, ctx, dockerfileTemplate, cfg)
	if err != nil {
		return nil, err
	}

	var containers []*Container
	for i := 0; i < scale; i++ {
		if c, err := createContainer(cli, ctx, image, cfg); err != nil {
			return containers, err
		} else {
			containers = append(containers, c)
		}
	}

	// populate repository if needed
	err = populateRepo(scm, cfg)
	if err == nil {
		err = scm.Deploy(cfg.Namespace, cfg.Name, "")
	}

	return containers, err
}

func getScaling(cli DockerClient, ctx context.Context, name, namespace string, scale int) (int, error) {
	if scale <= 0 {
		return 0, fmt.Errorf("Invalid scaling value, it must be greater than 0")
	}

	cs, err := cli.FindApplications(ctx, name, namespace)
	if err != nil {
		return 0, err
	}

	n := len(cs)
	if scale <= n {
		return 0, fmt.Errorf("Application containers already reached maximum scaling value. "+
			"(maximum scaling = %d, existing containers = %d", scale, n)
	}

	return scale - n, nil
}

func populateRepo(scm SCM, cfg *createConfig) error {
	if strings.ToLower(cfg.Repo) == "empty" {
		return nil
	} else if cfg.Repo == "" {
		return populateFromTemplate(scm, cfg)
	} else {
		return scm.PopulateURL(cfg.Namespace, cfg.Name, cfg.Repo)
	}
}

func populateFromTemplate(scm SCM, cfg *createConfig) error {
	tpl := filepath.Join(cfg.Plugin.Path, "template")
	if fi, err := os.Stat(tpl); err != nil || !fi.IsDir() {
		return nil
	}

	f, err := files.TempFile("", "repo", ".tar")
	if err != nil {
		return err
	}
	defer func() {
		f.Close()
		os.Remove(f.Name())
	}()

	tw := tar.NewWriter(f)
	if err = archive.CopyFileTree(tw, "", tpl, nil, false); err != nil {
		return err
	}
	tw.Close()

	size, err := f.Seek(0, os.SEEK_CUR)
	f.Seek(0, os.SEEK_SET)
	if err == nil {
		err = scm.Populate(cfg.Namespace, cfg.Name, f, size)
	}
	return err
}

type serviceExistsError struct {
	service, app string
}

func (e serviceExistsError) Error() string {
	return fmt.Sprintf("%s: service already exists in '%s' application", e.service, e.app)
}

func (e serviceExistsError) HTTPErrorStatusCode() int {
	return http.StatusConflict
}

func createServiceContainer(cli DockerClient, ctx context.Context, cfg *createConfig) ([]*Container, error) {
	if cfg.ServiceName == "" {
		cfg.ServiceName = cfg.Plugin.Name
	}

	name, namespace, service := cfg.Name, cfg.Namespace, cfg.ServiceName
	cfg.Hostname = service + "." + name + "-" + namespace
	cfg.FQDN = cfg.Hostname + "." + defaults.Domain()
	cfg.Env["CLOUDWAY_APP_DNS"] = cfg.FQDN
	cfg.Env["CLOUDWAY_SERVICE_NAME"] = cfg.ServiceName

	cs, err := cli.FindService(ctx, name, namespace, service)
	if err != nil {
		return nil, err
	}
	if len(cs) != 0 {
		return nil, serviceExistsError{service: service, app: name}
	}

	image, err := buildImage(cli, ctx, dockerfileTemplate, cfg)
	if err != nil {
		return nil, err
	}

	c, err := createContainer(cli, ctx, image, cfg)
	if err != nil {
		return nil, err
	} else {
		return []*Container{c}, nil
	}
}

func buildImage(cli DockerClient, ctx context.Context, t *template.Template, cfg *createConfig) (imageId string, err error) {
	// Create temporary tar archive to create build context
	tarFile, err := ioutil.TempFile("", "docker")
	if err != nil {
		return
	}
	defer func() {
		tarFile.Close()
		os.Remove(tarFile.Name())
	}()

	tw := tar.NewWriter(tarFile)
	logrus.Debugf("Created temporary build context: %s", tarFile.Name())

	// create and add Dockerfile to archive
	dockerfile := createDockerfile(t, cfg)
	if err = archive.AddFile(tw, "Dockerfile", 0644, dockerfile); err != nil {
		return
	}

	// copy application support files
	sandbox := filepath.Join(config.RootDir, "sandbox")
	if err = archive.CopyFileTree(tw, "sandbox", sandbox, nil, true); err != nil {
		return
	}

	// add plugin files
	if err = addPluginFiles(tw, cfg.PluginInstallPath, cfg.Plugin.Path); err != nil {
		return
	}

	// rewind the archive file for reading
	tw.Close()
	if _, err = tarFile.Seek(0, 0); err != nil {
		return
	}

	// build the image from context
	options := types.ImageBuildOptions{Dockerfile: "Dockerfile", Remove: true, ForceRemove: true}
	response, err := cli.ImageBuild(ctx, tarFile, options)

	// read image ID from build response
	if err == nil {
		defer response.Body.Close()
		imageId, err = readBuildStream(response.Body, cfg.Log)
	}

	// get actual image ID
	if err == nil {
		info, _, err := cli.ImageInspectWithRaw(ctx, imageId, false)
		if err == nil {
			imageId = info.ID
		}
	}

	return
}

func readBuildStream(in io.Reader, out io.Writer) (id string, err error) {
	const SUCCESS = "Successfully built "

	type Flusher interface {
		Flush()
	}

	type ErrFlusher interface {
		Flush() error
	}

	var enc *json.Encoder
	if out != nil {
		enc = json.NewEncoder(out)
	}

	var dec = json.NewDecoder(in)
	for {
		var jm JSONMessage
		if er := dec.Decode(&jm); er != nil {
			if er != io.EOF {
				err = er
			}
			break
		}

		if jm.Stream != "" {
			os.Stdout.WriteString(jm.Stream)
			if out != nil {
				err = enc.Encode(&apitypes.ServerLog{Message: jm.Stream})
				if err == nil {
					switch b := out.(type) {
					case Flusher:
						b.Flush()
					case ErrFlusher:
						err = b.Flush()
					}
				}
				if err != nil {
					break
				}
			}
		}

		if jm.Error != nil {
			err = jm.Error
		} else if strings.HasPrefix(jm.Stream, SUCCESS) {
			id = strings.TrimSpace(jm.Stream[len(SUCCESS):])
		}
	}

	if id == "" && err == nil {
		err = errors.New("create: no image ID read from build response")
	}

	return id, err
}

func createDockerfile(t *template.Template, cfg *createConfig) []byte {
	b, err := archive.ReadFile(cfg.Plugin.Path, "bin/install")
	if err == nil {
		script := strings.Replace(string(b), "\n", "\\n\\\n", -1)
		script = strings.Replace(script, "'", "'\\''", -1)
		cfg.InstallScript = script
	} else {
		cfg.InstallScript = ""
	}

	var buf bytes.Buffer
	if err = t.Execute(&buf, cfg); err != nil {
		panic(err)
	}
	if cfg.Debug {
		fmt.Println(buf.String())
	}
	return buf.Bytes()
}

func addPluginFiles(tw *tar.Writer, dst, path string) error {
	stat, err := os.Stat(path)
	if err != nil {
		return err
	}
	if stat.IsDir() {
		return archive.CopyFileTree(tw, dst, path, nil, false)
	} else {
		return archive.CopyFile(tw, path, dst, 0)
	}
}

func createContainer(cli DockerClient, ctx context.Context, imageId string, cfg *createConfig) (*Container, error) {
	config := &container.Config{
		Image: imageId,
		Labels: map[string]string{
			CATEGORY_KEY:      string(cfg.Category),
			PLUGIN_KEY:        cfg.Plugin.Name + ":" + cfg.Plugin.Version,
			APP_NAME_KEY:      cfg.Name,
			APP_NAMESPACE_KEY: cfg.Namespace,
			APP_HOME_KEY:      cfg.Home,
		},
		User:       cfg.User,
		Entrypoint: strslice.StrSlice([]string{"/usr/bin/cwctl", "run"}),
	}

	if cfg.Category.IsService() {
		config.Hostname = cfg.Hostname
		config.Labels[SERVICE_NAME_KEY] = cfg.ServiceName
	}

	if cfg.DependsOn != nil {
		config.Labels[SERVICE_DEPENDS_KEY] = strings.Join(cfg.DependsOn, ",")
	}

	hostConfig := &container.HostConfig{}
	netConfig := &network.NetworkingConfig{}

	if cfg.Network != "" {
		hostConfig.NetworkMode = container.NetworkMode(cfg.Network)
	}

	var baseName = cfg.Name + "-" + cfg.Namespace + "-"
	if cfg.ServiceName != "" {
		baseName = cfg.ServiceName + "." + baseName
	}

	var containerName string
	for i := 1; ; i++ {
		containerName = baseName + strconv.Itoa(i)
		_, err := cli.ContainerInspect(ctx, containerName)
		if err != nil {
			break
		}
	}

	resp, err := cli.ContainerCreate(ctx, config, hostConfig, netConfig, containerName)
	if err != nil {
		logrus.WithError(err).Error("failed to create container")
		return nil, err
	}
	c, err := cli.Inspect(ctx, resp.ID)
	if err != nil {
		return nil, err
	}

	if len(cfg.Hosts) != 0 {
		hosts := cfg.Hosts
		if cfg.Category.IsService() {
			shosts := make([]string, len(hosts))
			for i := range hosts {
				shosts[i] = cfg.ServiceName + "." + hosts[i]
			}
			hosts = shosts
		}
		c.AddHost(ctx, hosts[0], hosts[1:]...)
	}

	return c, nil
}

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

	"github.com/cloudway/platform/api"
	"github.com/cloudway/platform/config"
	"github.com/cloudway/platform/config/defaults"
	"github.com/cloudway/platform/pkg/archive"
	"github.com/cloudway/platform/pkg/manifest"
)

type CreateOptions struct {
	Name        string
	Namespace   string
	ServiceName string
	Plugin      *manifest.Plugin
	Image       string
	Flags       uint32
	Secret      string
	Home        string
	User        string
	Network     string
	Capacity    string
	Scaling     int
	Hosts       []string
	Env         map[string]string
	Repo        string
	OutLog      io.Writer
	ErrLog      io.Writer
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
func (cli DockerClient) Create(ctx context.Context, opts CreateOptions) ([]*Container, error) {
	cfg := configure(&opts)

	switch cfg.Category {
	case manifest.Framework:
		return createApplicationContainer(cli, ctx, cfg)
	case manifest.Service:
		return createServiceContainer(cli, ctx, cfg)
	default:
		return nil, fmt.Errorf("%s:%s is not a valid plugin", cfg.Plugin.Name, cfg.Plugin.Version)
	}
}

func configure(opts *CreateOptions) *createConfig {
	cfg := &createConfig{CreateOptions: opts}
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

	// passthrough plugin specific environment variables from broker
	prefix := "CLOUDWAY_PLUGIN_" + strings.ToUpper(cfg.Plugin.Name) + "_"
	for _, e := range os.Environ() {
		kv := strings.SplitN(e, "=", 2)
		if strings.HasPrefix(kv[0], prefix) {
			k := kv[0][len(prefix):]
			if _, exists := cfg.Env[k]; !exists {
				cfg.Env[k] = kv[1]
			}
		}
	}
	for k, v := range config.GetSection("plugin:" + cfg.Plugin.Name) {
		k = strings.ToUpper(k)
		if _, exists := cfg.Env[k]; !exists {
			cfg.Env[k] = v
		}
	}

	return cfg
}

// Create a builder container.
func (cli DockerClient) CreateBuilder(ctx context.Context, opts CreateOptions) (c *Container, err error) {
	cfg := configure(&opts)

	cfg.Hostname = cfg.Name + "-" + cfg.Namespace
	cfg.FQDN = cfg.Hostname + "." + defaults.Domain()
	cfg.Env["CLOUDWAY_APP_DNS"] = cfg.FQDN

	err = buildImage(cli, ctx, dockerfileTemplate, cfg)
	if err != nil {
		return nil, err
	}

	return createBuilderContainer(cli, ctx, cfg)
}

func createApplicationContainer(cli DockerClient, ctx context.Context, cfg *createConfig) ([]*Container, error) {
	if cfg.ServiceName != "" {
		return nil, fmt.Errorf("The application name cannot contains a serivce name: %s", cfg.ServiceName)
	}

	cfg.Hostname = cfg.Name + "-" + cfg.Namespace
	cfg.FQDN = cfg.Hostname + "." + defaults.Domain()
	cfg.Env["CLOUDWAY_APP_DNS"] = cfg.FQDN

	if _, e := os.Stat(filepath.Join(cfg.Plugin.Path, "bin", "build")); os.IsNotExist(e) {
		// If the framework has no build script, the application ca be hot deployed
		cfg.Flags |= HotDeployable
	}

	scale, err := getScaling(cli, ctx, cfg.Name, cfg.Namespace, cfg.Scaling)
	if err != nil {
		return nil, err
	}

	err = buildImage(cli, ctx, dockerfileTemplate, cfg)
	if err != nil {
		return nil, err
	}

	var containers []*Container
	for i := 0; i < scale; i++ {
		if c, err := createContainer(cli, ctx, cfg); err != nil {
			return containers, err
		} else {
			containers = append(containers, c)
		}
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

	err = buildImage(cli, ctx, dockerfileTemplate, cfg)
	if err != nil {
		return nil, err
	}

	c, err := createContainer(cli, ctx, cfg)
	if err != nil {
		return nil, err
	} else {
		return []*Container{c}, nil
	}
}

func buildImage(cli DockerClient, ctx context.Context, t *template.Template, cfg *createConfig) (err error) {
	if cfg.Image != "" {
		return
	}

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
	var imageId string
	if err == nil {
		defer response.Body.Close()
		imageId, err = readBuildStream(response.Body, cfg.OutLog)
	}

	// get actual image ID
	if err == nil {
		info, _, err := cli.ImageInspectWithRaw(ctx, imageId, false)
		if err == nil {
			imageId = info.ID
		}
	}

	cfg.Image = imageId
	return
}

func readBuildStream(in io.Reader, out io.Writer) (id string, err error) {
	const SUCCESS = "Successfully built "

	var dec = json.NewDecoder(in)
	for {
		var jm JSONMessage
		if er := dec.Decode(&jm); er != nil {
			if er != io.EOF {
				err = er
			}
			break
		}

		if jm.Stream != "" && out != nil {
			if _, err = out.Write([]byte(jm.Stream)); err != nil {
				break
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

func createContainer(cli DockerClient, ctx context.Context, cfg *createConfig) (*Container, error) {
	config := &container.Config{
		Labels: map[string]string{
			VERSION_KEY:       api.Version,
			CATEGORY_KEY:      string(cfg.Category),
			PLUGIN_KEY:        cfg.Plugin.Tag,
			FLAGS_KEY:         strconv.FormatUint(uint64(cfg.Flags), 10),
			APP_NAME_KEY:      cfg.Name,
			APP_NAMESPACE_KEY: cfg.Namespace,
			APP_HOME_KEY:      cfg.Home,
		},

		Image:      cfg.Image,
		User:       cfg.User,
		Entrypoint: strslice.StrSlice{"/usr/bin/cwctl", "run"},
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

func createBuilderContainer(cli DockerClient, ctx context.Context, cfg *createConfig) (*Container, error) {
	config := &container.Config{
		Image:      cfg.Image,
		User:       cfg.User,
		Entrypoint: strslice.StrSlice{"/usr/bin/cwctl", "run"},
	}

	hostConfig := &container.HostConfig{}
	netConfig := &network.NetworkingConfig{}

	if cfg.Network != "" {
		hostConfig.NetworkMode = container.NetworkMode(cfg.Network)
	}

	resp, err := cli.ContainerCreate(ctx, config, hostConfig, netConfig, "")
	if err != nil {
		logrus.WithError(err).Error("failed to create container")
		return nil, err
	}

	info, err := cli.ContainerInspect(ctx, resp.ID)
	if err != nil {
		return nil, err
	}

	return &Container{
		Name:          cfg.Name,
		Namespace:     cfg.Namespace,
		DockerClient:  cli,
		ContainerJSON: &info,
	}, nil
}

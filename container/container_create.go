package container

import (
    "fmt"
    "os"
    "io"
    "errors"
    "strings"
    "bytes"
    "path/filepath"
    "io/ioutil"
    "archive/tar"
    "encoding/json"
    "text/template"

    "golang.org/x/net/context"
    "github.com/Sirupsen/logrus"
    "github.com/docker/engine-api/client"
    "github.com/docker/engine-api/types"
    "github.com/docker/engine-api/types/container"
    "github.com/docker/engine-api/types/network"
    "github.com/docker/engine-api/types/strslice"

    "github.com/cloudway/platform/plugin"
    "github.com/cloudway/platform/container/conf"
    "github.com/cloudway/platform/container/conf/defaults"
    "github.com/cloudway/platform/container/archive"
)

type CreateOptions struct {
    Name            string
    Namespace       string
    Category        plugin.Category
    ServiceName     string
    Home            string
    User            string
    Hostname        string
    FQDN            string
    Capacity        string
    Scaling         int
    PluginSource    string
    PluginName      string
    PluginPath      string
    BaseImage       string
    InstallScript   string
    Debug           bool
}

// Create new application containers.
func Create(config CreateOptions) ([]*Container, error) {
    cli, err := docker_client()
    if err != nil {
        return nil, err
    }

    meta, err := archive.ReadManifest(config.PluginSource)
    if err != nil {
        return nil, err
    }

    if config.User == "" {
        if meta.User != "" {
            config.User = meta.User
        } else {
            config.User = defaults.AppUser()
        }
    }
    if config.User == "root" {
        config.Home = "/root"
    } else {
        config.Home = defaults.AppHome()
    }

    config.Category   = meta.Category
    config.PluginName = meta.Name
    config.PluginPath = filepath.Base(meta.Path)
    config.BaseImage  = meta.BaseImage
    config.Debug      = DEBUG

    switch config.Category {
    case plugin.Framework:
        return createApplicationContainer(cli, meta, config)
    case plugin.Service:
        return createServiceContainer(cli, meta, config)
    default:
        return nil, fmt.Errorf("%s is not a valid plugin", meta.Path)
    }
}

func createApplicationContainer(cli *client.Client, plugin *plugin.Plugin, config CreateOptions) ([]*Container, error) {
    if config.ServiceName != "" {
        return nil, fmt.Errorf("The application name cannot contains a serivce name: %s", config.ServiceName)
    }

    config.Hostname = config.Name + "-" + config.Namespace
    config.FQDN = config.Hostname + "." + defaults.Domain()

    scale, err := getScaling(config.Name, config.Namespace, config.Scaling)
    if err != nil {
        return nil, err
    }

    image, err := buildImage(cli, dockerfileTemplate, plugin, config)
    if err != nil {
        return nil, err
    }

    var containers []*Container
    for i := 0; i < scale; i++ {
        if c, err := createContainer(cli, image, config); err != nil {
            return nil, err
        } else {
            containers = append(containers, c)
        }
    }

    return containers, nil
}

func getScaling(name, namespace string, scale int) (int, error) {
    if scale <= 0 {
        return 0, fmt.Errorf("Invalid scaling value, it must be greater than 0")
    }

    cs, err := FindApplications(name, namespace)
    if err != nil {
        return 0, err
    }

    n := len(cs)
    if scale <= n {
        return 0, fmt.Errorf("Application containers already reached maximum scaling value. " +
                             "(maximum scaling = %d, existing containers = %d", scale, n)
    }

    return scale - n, nil
}

func createServiceContainer(cli *client.Client, plugin *plugin.Plugin, config CreateOptions) ([]*Container, error) {
    if config.ServiceName == "" {
        config.ServiceName = plugin.Name
    }

    name, namespace, service := config.Name, config.Namespace, config.ServiceName
    config.Hostname = service + "." + name + "-" + namespace
    config.FQDN = config.Hostname + "." + defaults.Domain()

    cs, err := FindService(name, namespace, service)
    if err != nil {
        return nil, err
    }
    if len(cs) != 0 {
        return nil, fmt.Errorf("Service already installed: %s", service)
    }

    image, err := buildImage(cli, dockerfileTemplate, plugin, config)
    if err != nil {
        return nil, err
    }

    c, err := createContainer(cli, image, config)
    if err != nil {
        return nil, err
    } else {
        return []*Container{c}, nil
    }
}

func buildImage(cli *client.Client, t *template.Template, plugin *plugin.Plugin, config CreateOptions) (image string, err error) {
    // Create temporary tar archive to create build context
    tarFile, err := ioutil.TempFile("", "docker");
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
    dockerfile := createDockerfile(t, plugin, config)
    if err = archive.AddFile(tw, "Dockerfile", 0644, dockerfile); err != nil {
        return
    }

    // copy application support files
    support := filepath.Join(conf.RootDir, "libexec")
    if err = archive.CopyFileTree(tw, "support", support, true); err != nil {
        return
    }

    // add plugin files
    if err = addPluginFiles(tw, plugin.Path); err != nil {
        return
    }

    // rewind the archive file for reading
    tw.Close()
    if _, err = tarFile.Seek(0, 0); err != nil {
        return
    }

    // build the image from context
    options := types.ImageBuildOptions{Dockerfile: "Dockerfile", Remove: true, ForceRemove: true}
    response, err := cli.ImageBuild(context.Background(), tarFile, options);
    if err == nil {
        defer response.Body.Close()
        image, err = readBuildImageId(response.Body)
    }
    return
}

func readBuildImageId(in io.Reader) (id string, err error) {
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

        if jm.Stream != "" {
            fmt.Print(jm.Stream)
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

func createDockerfile(t *template.Template, plugin *plugin.Plugin, config CreateOptions) []byte{
    b, err := archive.ReadFile(plugin.Path, "bin/install")
    if err == nil {
        script := strings.Replace(string(b), "\n", "\\n\\\n", -1)
        script  = strings.Replace(script, "\"", "\\\"", -1)
        config.InstallScript = script
    } else {
        config.InstallScript = ""
    }

    var buf bytes.Buffer
    if err = t.Execute(&buf, config); err != nil {
        panic(err)
    }
    if DEBUG {
        fmt.Println(buf.String())
    }
    return buf.Bytes()
}

func addPluginFiles(tw *tar.Writer, path string) error {
    stat, err := os.Stat(path)
    if err != nil {
        return err
    }
    if stat.IsDir() {
        return archive.CopyFileTree(tw, filepath.Base(path), path, false)
    } else {
        return archive.CopyFile(tw, path, filepath.Base(path), 0)
    }
}

func createContainer(cli *client.Client, imageId string, options CreateOptions) (*Container, error) {
    config := &container.Config {
        Image: imageId,
        Labels: map[string]string {
            CATEGORY_KEY:      string(options.Category),
            APP_NAME_KEY:      options.Name,
            APP_NAMESPACE_KEY: options.Namespace,
            APP_HOME_KEY:      options.Home,
        },
        User: options.User,
        Entrypoint: strslice.StrSlice{"/usr/bin/cwctl", "run"},
    }

    if options.Category == plugin.Service {
        config.Hostname = options.Hostname
        config.Labels[SERVICE_NAME_KEY] = options.ServiceName
    }

    hostConfig := &container.HostConfig{}
    netConfig := &network.NetworkingConfig{}

    resp, err := cli.ContainerCreate(context.Background(), config, hostConfig, netConfig, "")
    if err != nil {
        return nil, err
    }
    return Inspect(cli, resp.ID)
}

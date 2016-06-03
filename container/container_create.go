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

    "golang.org/x/net/context"
    "github.com/docker/engine-api/client"
    "github.com/docker/engine-api/types"
    "github.com/docker/engine-api/types/container"
    "github.com/docker/engine-api/types/network"
    "github.com/docker/engine-api/types/strslice"

    "github.com/cloudway/platform/container/conf/defaults"
    "github.com/Sirupsen/logrus"
    "github.com/cloudway/platform/plugin"
)

// Create a new application container.
func Create(config map[string]string) (*Container, error) {
    cli, err := docker_client()
    if err != nil {
        return nil, err
    }

    plugin, err := readManifest(config["source"])
    if err != nil {
        return nil, err
    }

    image, err := buildImage(cli, plugin, config)
    if err != nil {
        return nil, err
    }

    return createContainer(cli, image, config)
}

func buildImage(cli *client.Client, plugin *plugin.Plugin, config map[string]string) (image string, err error) {
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
    if err = addFile(tw, "Dockerfile", 0644, createDockerfile(plugin, config)); err != nil {
        return
    }

    // add plugin files
    if err = addPluginFiles(tw, plugin.Path, plugin.Name); err != nil {
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

        if DEBUG && jm.Stream != "" {
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

func createDockerfile(plugin *plugin.Plugin, config map[string]string) []byte {
    name, namespace := config["name"], config["namespace"]
    if name == "" || namespace == "" {
        panic("create: no name and/or namespace provided")
    }

    home := getOrDefault(config, "home", defaults.AppHome())
    user := getOrDefault(config, "user", defaults.AppUser())

    // populating the Dockerfile contents
    buf := new(bytes.Buffer)

    fmt.Fprintf(buf, "FROM %s\n", plugin.BaseImage)

    // Begin of RUN commands, make sure RUN commands contiguous
    fmt.Fprintf(buf, "RUN ")

    // create operating system user and group
    fmt.Fprintf(buf, "groupadd %[1]s && useradd -g %[1]s -d %[2]s -m %[1]s", user, home)

    // create home directory
    fmt.Fprint(buf, " && mkdir -p")
    for _, d := range []string{".env", "app/logs", "app/data", "app/repo"} {
        fmt.Fprintf(buf, " %s/%s", home, d)
    }

    // set directory permissions
    fmt.Fprintf(buf, " && chown root:root %[2]s/.env && " +
                "chown -R %[1]s:%[1]s %[2]s/app && " +
                "chown root:%[1]s %[2]s/app\n",
                user, home)
    // End of RUN commands

    fmt.Fprintf(buf, "WORKDIR %s\n", home)

    // add environment variables
    env := map[string]string {
        "CLOUDWAY_APP_NAME":      name,
        "CLOUDWAY_APP_NAMESPACE": namespace,
        "CLOUDWAY_APP_DNS":       name + "-" + namespace + "." + defaults.Domain(),
        "CLOUDWAY_HOME_DIR":      home,
        "CLOUDWAY_LOG_DIR":       home + "/app/logs",
        "CLOUDWAY_DATA_DIR":      home + "/app/data",
        "CLOUDWAY_REPO_DIR":      home + "/app/repo",
    }

    fmt.Fprint(buf, "ENV")
    for k, v := range env {
        fmt.Fprintf(buf, " %s=%q", k, v)
    }
    fmt.Fprintln(buf)

    // add plugin files
    fmt.Fprintf(buf, "ADD %s %s/%s\n", filepath.Base(plugin.Path), home, plugin.Name)
    fmt.Fprintf(buf, "RUN chown -R root:%s %s/%s\n", user, home, plugin.Name)

    return buf.Bytes()
}

func addPluginFiles(tw *tar.Writer, src, dst string) error {
    pf, err := os.Open(src)
    if err != nil {
        return err
    }
    defer pf.Close()

    stat, err := pf.Stat()
    if err != nil {
        return err
    }

    if stat.IsDir() {
        return copyFileTree(tw, src, dst)
    } else {
        return copyFile(tw, filepath.Base(src), int64(stat.Mode()), stat.Size(), pf)
    }
}

func createContainer(cli *client.Client, imageId string, config map[string]string) (c *Container, err error) {
    options := &container.Config {
        Image: imageId,
        Labels: map[string]string {
            _APP_NAME_KEY:      config["name"],
            _APP_NAMESPACE_KEY: config["namespace"],
            _APP_HOME_KEY:      getOrDefault(config, "home", defaults.AppHome()),
            _APP_CAPACITY_KEY:  getOrDefault(config, "capacity", defaults.AppCapacity()),
        },
        User: getOrDefault(config, "user", defaults.AppUser()),
        Cmd: strslice.StrSlice{"/bin/sh", "-c", "while :; do sleep 32768; done"}, // FIXME
    }

    resp, err := cli.ContainerCreate(context.Background(), options, &container.HostConfig{}, &network.NetworkingConfig{}, "")
    if err != nil {
        return
    }
    return fromId(cli, resp.ID)
}

func getOrDefault(config map[string]string, key string, deflt string) string {
    if value, ok := config[key]; ok {
        return value
    } else {
        return deflt
    }
}
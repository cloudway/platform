package container

import (
    "fmt"
    "os"
    "io"
    "errors"
    "strings"
    "bytes"
    "io/ioutil"
    "path/filepath"
    "archive/tar"
    "encoding/json"

    "golang.org/x/net/context"
    "github.com/docker/engine-api/types"
    "github.com/docker/engine-api/types/container"
    "github.com/docker/engine-api/types/network"
    "github.com/docker/engine-api/types/strslice"

    "icloudway.com/platform/container/conf/defaults"
)

// Create a new application container.
func Create(config map[string]string) (*Container, error) {
    image, err := buildImage(config)
    if err != nil {
        return nil, err
    }
    return createContainer(image, config)
}

func buildImage(config map[string]string) (image string, err error) {
    cli, err := docker_client()
    if err != nil {
        return
    }

    // Create temporary tar archive to create build context
    tarFile, err := ioutil.TempFile("", "docker");
    if err != nil {
        return
    }
    defer os.Remove(tarFile.Name())
    tw := tar.NewWriter(tarFile)

    if (DEBUG) {
        fmt.Printf("created temporary build context: %s\n", tarFile.Name())
    }

    // create and add Dockerfile to archive
    err = addFile(tw, "Dockerfile", 0644, createDockerfile(config))
    if err != nil {
        return
    }

    // close the archive file for reading
    tw.Close()
    ctx, err := os.Open(tarFile.Name())
    if err != nil {
        return
    }
    defer ctx.Close()

    // build the image from context
    options := types.ImageBuildOptions{Dockerfile: "Dockerfile", Remove: true, ForceRemove: true}
    response, err := cli.ImageBuild(context.Background(), ctx, options);
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

func addFile(tw *tar.Writer, filename string, filemode int64, content []byte) error {
    hdr := &tar.Header {
        Name: filename,
        Mode: filemode,
        Size: int64(len(content)),
    }

    err := tw.WriteHeader(hdr)
    if err == nil {
        _, err = tw.Write(content)
    }
    return err
}

func createDockerfile(config map[string]string) []byte {
    name := config["name"]
    namespace := config["namespace"]
    if name == "" || namespace == "" {
        panic("create: no name and/or namespace provided")
    }

    home := getOrDefault(config, "home", defaults.AppHome())
    user := getOrDefault(config, "user", defaults.AppUser())

    // populating the Dockerfile contents
    buf := new(bytes.Buffer)

    fmt.Fprintf(buf, "FROM %s\n", config["baseImage"])

    // mark the container image
    fmt.Fprintf(buf, "LABEL com.cloudway.image.version=1\n")

    // create operating system user and group
    fmt.Fprintf(buf, "RUN groupadd %[1]s && useradd -g %[1]s -d %[2]s -m %[1]s\n", user, home)

    // create home directory
    fmt.Fprint(buf, "RUN mkdir -p")
    for _, d := range []string{".env", "app/logs", "app/data", "app/repo"} {
        fmt.Fprintf(buf, " %s", filepath.Join(home, d))
    }
    fmt.Fprintln(buf)
    fmt.Fprintf(buf, "WORKDIR %s\n", home)

    // set directory permissions
    fmt.Fprintf(buf, "RUN chown root:root %[2]s/.env && " +
                "chown -R %[1]s:%[1]s %[2]s/app && " +
                "chown root:%[1]s %[2]s/app\n",
                user, home)

    // add environment variables
    env := map[string]string {
        "CLOUDWAY_APP_NAME":      name,
        "CLOUDWAY_APP_NAMESPACE": namespace,
        "CLOUDWAY_APP_DNS":       name + "-" + namespace + "." + defaults.Domain(),
        "CLOUDWAY_HOME_DIR":      home,
        "CLOUDWAY_LOG_DIR":       filepath.Join(home, "/app/logs"),
        "CLOUDWAY_DATA_DIR":      filepath.Join(home, "/app/data"),
        "CLOUDWAY_REPO_DIR":      filepath.Join(home, "/app/repo"),
    }

    fmt.Fprint(buf, "ENV")
    for k, v := range env {
        fmt.Fprintf(buf, " %s=%q", k, v)
    }
    fmt.Fprintln(buf)

    return buf.Bytes()
}

func createContainer(imageId string, config map[string]string) (c *Container, err error) {
    cli, err := docker_client()
    if err != nil {
        return
    }

    options := &container.Config {
        Image: imageId,
        Labels: map[string]string {
            _APP_NAME_KEY:      config["name"],
            _APP_NAMESPACE_KEY: config["namespace"],
            _APP_HOME_KEY:      getOrDefault(config, "home", defaults.AppHome()),
            _APP_CAPACITY_KEY:  getOrDefault(config, "capacity", defaults.AppCapacity()),
            _IMAGE_ID_KEY:      imageId,
        },
        User: getOrDefault(config, "user", defaults.AppUser()),
        Cmd: strslice.StrSlice{"/bin/sh", "-c", "while :; do sleep 32768; done"}, // FIXME
    }

    resp, err := cli.ContainerCreate(context.Background(), options, &container.HostConfig{}, &network.NetworkingConfig{}, "")
    if err != nil {
        return
    }
    return FromId(resp.ID)
}

func getOrDefault(config map[string]string, key string, deflt string) string {
    if value, ok := config[key]; ok {
        return value
    } else {
        return deflt
    }
}
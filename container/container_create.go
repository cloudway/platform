package container

import (
    "fmt"
    "os"
    "io"
    "errors"
    "bufio"
    "strings"
    "bytes"
    "io/ioutil"
    "path/filepath"
    "archive/tar"

    "golang.org/x/net/context"
    "github.com/docker/engine-api/types"
    "github.com/docker/engine-api/types/container"
    "github.com/docker/engine-api/types/network"
    "github.com/docker/engine-api/types/strslice"
)

// Create a new application container.
func Create(config map[string]string) (c *Container, err error) {
    image, err := buildImage(config)
    if err == nil {
        c, err = createContainer(image, config)
    }
    return c, err
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

func readBuildImageId(rd io.Reader) (string, error) {
    const SUCCESS = "Successfully built "

    in := bufio.NewReader(rd)
    for {
        // TODO: analyze response to check errors
        line, err := in.ReadString('\n')
        if i := strings.Index(line, SUCCESS); i >= 0 {
            if j := strings.LastIndex(line, "\\n"); j > i {
                return line[i + len(SUCCESS) : j], nil
            }
        }
        if err == io.EOF {
            err = errors.New("create: no image ID read from build response")
        }
        if err != nil {
            return "", err
        }
    }
}

func addFile(tw *tar.Writer, filename string, filemode int64, content []byte) (error) {
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

    home := getOrDefault(config, "home", _DEFAULT_HOME)
    user := getOrDefault(config, "user", _DEFAULT_USER)

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
        "CLOUDWAY_APP_DNS":       name + "-" + namespace + "." + _DEFAULT_DOMAIN,
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

    if (DEBUG) {
        fmt.Println("Dockerfile contents")
        fmt.Println(string(buf.Bytes()))
        fmt.Println()
    }

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
            _APP_HOME_KEY:      getOrDefault(config, "home", _DEFAULT_HOME),
            _APP_CAPACITY_KEY:  getOrDefault(config, "capacity", _DEFAULT_CAPACITY),
            _IMAGE_ID_KEY:      imageId,
        },
        User: getOrDefault(config, "user", _DEFAULT_USER),
        Cmd: strslice.StrSlice{"/bin/sh", "-c", "while :; do sleep 32768; done"}, // FIXME
    }

    resp, err := cli.ContainerCreate(context.Background(), options, &container.HostConfig{}, &network.NetworkingConfig{}, "")
    if err == nil {
        c, err = FromId(resp.ID)
    }
    return c, err
}

func getOrDefault(config map[string]string, key string, deflt string) string {
    if value, ok := config[key]; ok {
        return value
    } else {
        return deflt
    }
}
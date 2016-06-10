package cmds

import (
    "io"
    "os"
    "bytes"
    "encoding/json"
    "archive/tar"
    "github.com/spf13/cobra"
    "github.com/Sirupsen/logrus"
    "github.com/docker/engine-api/client"
    "github.com/docker/engine-api/types"
    "github.com/docker/engine-api/types/filters"
    "github.com/docker/engine-api/types/events"
    "github.com/cloudway/platform/container"
    "github.com/cloudway/platform/proxy"
    "github.com/cloudway/platform/plugin"
    "golang.org/x/net/context"
)

func init() {
    cmdUpdateProxy := &cobra.Command{
        Use:        "update-proxy",
        Short:      "Update reverse proxy",
        Run:        runUpdateProxyCmd,
        Hidden:     true,
    }
    RootCommand.AddCommand(cmdUpdateProxy)
}

func runUpdateProxyCmd(cmd *cobra.Command, args []string) {
    cli, err := client.NewEnvClient()
    check(err)

    filters := filters.NewArgs()
    filters.Add("type", "container")
    filters.Add("event", "start")
    filters.Add("event", "die")
    filters.Add("event", "destroy")
    filters.Add("label", container.APP_NAME_KEY)
    filters.Add("label", container.APP_NAMESPACE_KEY)

    resp, err := cli.Events(context.Background(), types.EventsOptions{Filters: filters})
    check(err)
    defer resp.Close()

    proxy, err := proxy.New(os.Getenv("CLOUDWAY_PROXY_HOST"))
    check(err)
    defer proxy.Close()

    dec := json.NewDecoder(resp)
    for {
        var event events.Message
        var err error

        err = dec.Decode(&event)
        if err == io.EOF {
            break
        }
        if err != nil {
            logrus.Error(err)
            continue
        }

        switch event.Action {
        case "start":
            logrus.Debugf("container started: %s", event.Actor.ID)
            c, err := container.Inspect(cli, event.Actor.ID)
            if err == nil {
                err = handleStart(proxy, c)
            }

        case "die", "destroy":
            err = handleStop(proxy, event.Actor.ID)
        }

        if err != nil {
            logrus.Error(err)
        }
    }
}

func handleStart(proxy proxy.Proxy, c *container.Container) error {
    // reterieve application info from container
    info, err := c.GetInfo()
    if err != nil {
        return err
    }

    // add endpoints to the proxy server
    if err = proxy.AddEndpoints(c.ID, info.Endpoints); err != nil {
        return err
    }

    // distribute environments to other containers
    if err = distributeEnv(c, info.Env); err != nil {
        return err
    }

    return nil
}

func handleStop(proxy proxy.Proxy, id string) error {
    err := proxy.RemoveEndpoints(id)
    if err != nil {
        return err
    }
    return nil
}

func distributeEnv(c *container.Container, env map[string]string) error {
    if c.Category() != plugin.Service || len(env) == 0 {
        return nil
    }

    // Create an archive that contains all exported environment files
    buf := createEnvFile(env)

    // Write environments to all containers in the application
    cs, err := container.FindAll(c.Name, c.Namespace)
    if err != nil {
        return err
    }

    ctx := context.Background()
    for _, cc := range cs {
        if cc.ID != c.ID {
            err := cc.CopyToContainer(ctx, cc.ID, cc.EnvDir(), bytes.NewReader(buf), types.CopyToContainerOptions{})
            if err != nil {
                logrus.Error(err)
            }
        }
    }

    return nil
}

func createEnvFile(env map[string]string) []byte {
    buf := &bytes.Buffer{}
    tw := tar.NewWriter(buf)

    for name, value := range env {
        hdr := tar.Header{
            Name: name,
            Size: int64(len(value)),
            Mode: 0644,
        }
        tw.WriteHeader(&hdr)
        tw.Write([]byte(value))
    }

    tw.Close()
    return buf.Bytes()
}

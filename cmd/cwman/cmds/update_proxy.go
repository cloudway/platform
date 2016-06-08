package cmds

import (
    "io"
    "os"
    "bytes"
    "encoding/json"
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
    // run a container command to reterieve endpoint info
    var buf bytes.Buffer
    err := c.ExecE("root", nil, &buf, "/usr/bin/cwctl", "endpoints", "--ip", c.IP())
    if err != nil {
        return err
    }

    var endpoints []*plugin.Endpoint
    dec := json.NewDecoder(&buf)
    if err = dec.Decode(&endpoints); err != nil {
        return err
    }

    err = proxy.AddEndpoints(c.ID, endpoints)
    if err != nil {
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

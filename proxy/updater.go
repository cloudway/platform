package proxy

import (
    "io"
    "encoding/json"
    "github.com/Sirupsen/logrus"
    "golang.org/x/net/context"
    "github.com/docker/engine-api/client"
    "github.com/docker/engine-api/types"
    "github.com/docker/engine-api/types/filters"
    "github.com/docker/engine-api/types/events"
    "github.com/cloudway/platform/container"
)

func RunUpdater(cli *client.Client, proxy Proxy) error {
    var err error

    filters := filters.NewArgs()
    filters.Add("type", "container")
    filters.Add("event", "start")
    filters.Add("event", "die")
    filters.Add("event", "destroy")
    filters.Add("label", container.APP_NAME_KEY)
    filters.Add("label", container.APP_NAMESPACE_KEY)

    resp, err := cli.Events(context.Background(), types.EventsOptions{Filters: filters})
    if err != nil {
        return err
    }
    defer resp.Close()

    dec := json.NewDecoder(resp)
    for {
        var event events.Message
        err = dec.Decode(&event)
        if err == io.EOF {
            break
        }
        if err != nil {
            return err
        }

        switch event.Action {
        case "start":
            logrus.Debugf("container started: %s", event.Actor.ID)
            c, err := container.Inspect(cli, event.Actor.ID)
            if err == nil {
                err = handleStart(proxy, c)
            }

        case "die", "destroy":
            logrus.Debugf("container stopped: %s", event.Actor.ID)
            err = handleStop(proxy, event.Actor.ID)
        }

        if err != nil {
            logrus.Error(err)
        }
    }

    return nil
}

func handleStart(proxy Proxy, c *container.Container) error {
    // reterieve application info from container
    info, err := c.GetInfo()
    if err != nil {
        return err
    }

    // add endpoints to the proxy server
    if err = proxy.AddEndpoints(c.ID, info.Endpoints); err != nil {
        return err
    }

    return nil
}

func handleStop(proxy Proxy, id string) error {
    return proxy.RemoveEndpoints(id)
}

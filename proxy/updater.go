package proxy

import (
    "io"
    "os"
    "os/signal"
    "syscall"
    "encoding/json"
    "github.com/Sirupsen/logrus"
    "golang.org/x/net/context"
    "github.com/docker/engine-api/types"
    "github.com/docker/engine-api/types/filters"
    "github.com/docker/engine-api/types/events"
    "github.com/cloudway/platform/container"
    "github.com/cloudway/platform/container/conf"
    "github.com/cloudway/platform/pkg/manifest"
)

func RunUpdater(cli container.DockerClient, proxy Proxy) error {
    sigchan := make(chan os.Signal, 1)
    signal.Notify(sigchan, syscall.SIGHUP, syscall.SIGTERM, syscall.SIGINT)

    go func() {
        for {
            switch <- sigchan {
            case syscall.SIGHUP:
                logrus.Info("Reload mappings")
                if err := conf.Initialize(); err != nil {
                    logrus.Error(err)
                }
                if err := updateStaticMap(proxy); err != nil {
                    logrus.Error(err)
                }
            default:
                logrus.Info("Exiting")
                proxy.Close()
                os.Exit(0)
            }
        }
    }()

    if err := proxy.Reset(); err != nil {
        return err
    }
    if err := updateStaticMap(proxy); err != nil {
        return err
    }
    if err := rebuild(cli, proxy); err != nil {
        return err
    }
    if err := listen(cli, proxy); err != nil {
        return err
    }
    return nil
}

func updateStaticMap(proxy Proxy) error {
    var mappings []*manifest.ProxyMapping
    for frontend, backend := range conf.GetSection("proxy-mapping") {
        mappings = append(mappings, &manifest.ProxyMapping{
            Frontend: frontend,
            Backend:  backend,
            Protocol: "http",
        })
    }

    if len(mappings) != 0 {
        eps := []*manifest.Endpoint{
            &manifest.Endpoint{
                ProxyMappings: mappings,
            },
        }
        return proxy.AddEndpoints("static", eps)
    }

    return nil
}

func rebuild(cli container.DockerClient, proxy Proxy) error {
    ids, err := cli.IDs()
    if err != nil {
        return err
    }

    for _, id := range ids {
        c, err := cli.Inspect(id)
        if err == nil && c.State.Running && !c.State.Paused {
            err = handleStart(proxy, c)
        }
        if err != nil {
            logrus.Error(err)
            continue
        }
    }
    return nil
}

func listen(cli container.DockerClient, proxy Proxy) error {
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
            c, err := cli.Inspect(event.Actor.ID)
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

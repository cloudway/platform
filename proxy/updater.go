package proxy

import (
    "io"
    "os"
    "os/signal"
    "syscall"
    "strings"
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
                if err := update(proxy); err != nil {
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
    if err := update(proxy); err != nil {
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

func update(proxy Proxy) error {
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
    containers, err := cli.ContainerList(context.Background(), types.ContainerListOptions{})
    if err != nil {
        return err
    }

    for _, item := range containers {
        if item.Labels[container.APP_NAME_KEY] != "" && item.Labels[container.APP_NAMESPACE_KEY] != "" {
            c, err := cli.Inspect(item.ID)
            if err == nil {
                err = handleStart(proxy, c)
            }
            if err != nil {
                logrus.Error(err)
            }
        } else {
            info, err := cli.ContainerInspect(context.Background(), item.ID)
            if err == nil {
                err = handleVirtualHost(proxy, info)
            }
            if err != nil {
                logrus.Error(err)
            }
        }
    }
    return nil
}

func listen(cli container.DockerClient, proxy Proxy) error {
    var err error

    filters := filters.NewArgs()
    filters.Add("type",  "container")
    filters.Add("event", "start")
    filters.Add("event", "die")
    filters.Add("event", "destroy")

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
            _, ok1 := event.Actor.Attributes[container.APP_NAME_KEY]
            _, ok2 := event.Actor.Attributes[container.APP_NAMESPACE_KEY]
            if ok1 && ok2 {
                logrus.Debugf("container started: %s", event.Actor.ID)
                c, err := cli.Inspect(event.Actor.ID)
                if err == nil {
                    err = handleStart(proxy, c)
                }
            } else {
                info, err := cli.ContainerInspect(context.Background(), event.Actor.ID)
                if err == nil {
                    err = handleVirtualHost(proxy, info)
                }
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

func handleVirtualHost(proxy Proxy, info types.ContainerJSON) error {
    var vhost, vport string
    for _, env := range info.Config.Env {
        if strings.HasPrefix(env, "VIRTUAL_HOST=") {
            vhost = env[13:]
        }
        if strings.HasPrefix(env, "VIRTUAL_PORT=") {
            vport = env[13:]
        }
    }
    if vhost == "" {
        return nil
    }

    if vport == "" {
        ports := info.Config.ExposedPorts
        if len(ports) == 0 {
            return nil
        } else if len(ports) == 1 {
            for p := range ports {
                vport = p.Port()
            }
        } else {
            for p := range ports {
                if p.Port() == "80" || p.Port() == "8080" {
                    vport = p.Port()
                    break
                }
            }
        }
        if vport == "" {
            return nil
        }
    }

    ip  := info.NetworkSettings.IPAddress
    eps := []*manifest.Endpoint{&manifest.Endpoint{
        ProxyMappings: []*manifest.ProxyMapping{&manifest.ProxyMapping{
            Frontend:  vhost,
            Backend:  "http://" + ip + ":" + vport,
            Protocol: "http",
        }},
    }}
    return proxy.AddEndpoints(info.ID, eps)
}

func handleStop(proxy Proxy, id string) error {
    return proxy.RemoveEndpoints(id)
}

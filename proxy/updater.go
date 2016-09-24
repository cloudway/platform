package proxy

import (
	"context"
	"encoding/json"
	"io"
	"os"
	"os/signal"
	"strings"
	"syscall"

	"github.com/Sirupsen/logrus"
	"github.com/docker/engine-api/types"
	"github.com/docker/engine-api/types/events"
	"github.com/docker/engine-api/types/filters"

	"github.com/cloudway/platform/config"
	"github.com/cloudway/platform/container"
	"github.com/cloudway/platform/pkg/manifest"
)

func RunUpdater(cli container.DockerClient, proxy Proxy) error {
	sigchan := make(chan os.Signal, 1)
	signal.Notify(sigchan, syscall.SIGHUP, syscall.SIGTERM, syscall.SIGINT)

	go func() {
		for {
			switch <-sigchan {
			case syscall.SIGHUP:
				logrus.Info("Reload mappings")
				if err := config.Initialize(); err != nil {
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
	for frontend, backend := range config.GetSection("proxy-mapping") {
		mappings = append(mappings, &manifest.ProxyMapping{
			Frontend: frontend,
			Backend:  backend,
			Protocol: "http",
		})
	}

	if len(mappings) != 0 {
		eps := []*manifest.Endpoint{
			{
				ProxyMappings: mappings,
			},
		}
		return proxy.AddEndpoints("static", eps)
	}

	return nil
}

func rebuild(cli container.DockerClient, proxy Proxy) error {
	ctx := context.Background()

	containers, err := cli.ContainerList(ctx, types.ContainerListOptions{})
	if err != nil {
		return err
	}

	for _, item := range containers {
		if item.Labels[container.APP_NAME_KEY] != "" && item.Labels[container.APP_NAMESPACE_KEY] != "" {
			c, err := cli.Inspect(ctx, item.ID)
			if err == nil {
				err = handleStart(proxy, ctx, c)
			}
			if err != nil {
				logrus.Error(err)
			}
		} else {
			info, err := cli.ContainerInspect(ctx, item.ID)
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
	var ctx = context.Background()
	var err error

	filters := filters.NewArgs()
	filters.Add("type", "container")
	filters.Add("event", "start")
	filters.Add("event", "die")
	filters.Add("event", "destroy")

	resp, err := cli.Events(ctx, types.EventsOptions{Filters: filters})
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
				c, err := cli.Inspect(ctx, event.Actor.ID)
				if err == nil {
					err = handleStart(proxy, ctx, c)
				}
			} else {
				info, err := cli.ContainerInspect(ctx, event.Actor.ID)
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

func handleStart(proxy Proxy, ctx context.Context, c *container.Container) error {
	// reterieve application info from container
	info, err := c.GetInfo(ctx, "endpoints")
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
	// convert env to a map
	env := make(map[string]string)
	for _, entry := range info.Config.Env {
		parts := strings.SplitN(entry, "=", 2)
		if len(parts) == 2 {
			env[parts[0]] = parts[1]
		}
	}

	// find VIRTUAL_HOST env var(s)
	var mappings []*manifest.ProxyMapping
	for key := range env {
		if strings.HasPrefix(key, "VIRTUAL_HOST") {
			m := getVirtualHostMapping(proxy, key[12:], env, info)
			if m != nil {
				mappings = append(mappings, m)
			}
		}
	}

	if len(mappings) > 0 {
		eps := []*manifest.Endpoint{{ProxyMappings: mappings}}
		return proxy.AddEndpoints(info.ID, eps)
	}

	return nil
}

func getVirtualHostMapping(proxy Proxy, key string, env map[string]string, info types.ContainerJSON) *manifest.ProxyMapping {
	vhost := env["VIRTUAL_HOST"+key]
	vport := env["VIRTUAL_PORT"+key]

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

	ip := info.NetworkSettings.IPAddress
	if ip == "" {
		for _, net := range info.NetworkSettings.Networks {
			if net.IPAddress != "" {
				ip = net.IPAddress
				break
			}
		}
	}

	return &manifest.ProxyMapping{
		Frontend: vhost,
		Backend:  "http://" + ip + ":" + vport,
		Protocol: "http",
	}
}

func handleStop(proxy Proxy, id string) error {
	return proxy.RemoveEndpoints(id)
}

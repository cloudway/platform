package docker

import (
	"context"
	"fmt"
	"strconv"
	"strings"

	"github.com/docker/engine-api/client"
	"github.com/docker/engine-api/types"
	"github.com/docker/engine-api/types/filters"

	"github.com/cloudway/platform/config"
	"github.com/cloudway/platform/config/defaults"
	"github.com/cloudway/platform/container"
	"github.com/cloudway/platform/pkg/manifest"
)

const (
	APP_NAME_KEY        = "com.cloudway.app.name"
	APP_NAMESPACE_KEY   = "com.cloudway.app.namespace"
	APP_HOME_KEY        = "com.cloudway.app.home"
	VERSION_KEY         = "com.cloudway.container.version"
	CATEGORY_KEY        = "com.cloudway.container.category"
	PLUGIN_KEY          = "com.cloudway.container.plugin"
	FLAGS_KEY           = "com.cloudway.container.flags"
	SERVICE_NAME_KEY    = "com.cloudway.service.name"
	SERVICE_DEPENDS_KEY = "com.cloudway.service.depends"
)

const (
	HotDeployable uint32 = 1 << iota
)

// DockerEngine implements the docker engine.
type DockerEngine struct {
	*client.Client
}

// dockerContainer implements a docker container.
type dockerContainer struct {
	DockerEngine
	*types.ContainerJSON
}

func init() {
	container.NewEngine = func() (container.Engine, error) {
		cli, err := client.NewEnvClient()
		return DockerEngine{cli}, err
	}
}

func (cli DockerEngine) ServerVersion(ctx context.Context) (string, error) {
	v, err := cli.Client.ServerVersion(ctx)
	if err == nil {
		return v.Version, nil
	} else {
		return "", err
	}
}

// Returns an application container object constructed from the
// container id in the system.
func (cli DockerEngine) Inspect(ctx context.Context, id string) (container.Container, error) {
	info, err := cli.ContainerInspect(ctx, id)
	if err != nil {
		return nil, err
	}

	name := info.Config.Labels[APP_NAME_KEY]
	namespace := info.Config.Labels[APP_NAMESPACE_KEY]
	if name == "" || namespace == "" {
		return nil, fmt.Errorf("%s: Not a cloudway application container", id)
	}

	return &dockerContainer{
		DockerEngine:  cli,
		ContainerJSON: &info,
	}, nil
}

// FindInNamespace finds all containers in the given namespace. If the namespace
// is an empty string, then returns all containers in the system.
func (cli DockerEngine) FindInNamespace(ctx context.Context, namespace string) ([]container.Container, error) {
	return find(cli, ctx, "", "", "", namespace)
}

// Find all containers with the given name and namespace.
func (cli DockerEngine) FindAll(ctx context.Context, name, namespace string) ([]container.Container, error) {
	if name == "" || namespace == "" {
		return nil, nil
	}
	cs, err := find(cli, ctx, "", "", name, namespace)
	if err != nil {
		return cs, err
	}

	// reorder the container list
	var cs2, i = make([]container.Container, len(cs)), 0
	for _, c := range cs {
		if c.Category().IsFramework() {
			cs2[i] = c
			i++
		}
	}
	for _, c := range cs {
		if !c.Category().IsFramework() {
			cs2[i] = c
			i++
		}
	}
	return cs2, nil
}

// Find all application containers with the given name and namespace.
func (cli DockerEngine) FindApplications(ctx context.Context, name, namespace string) ([]container.Container, error) {
	if name == "" || namespace == "" {
		return nil, nil
	}
	return find(cli, ctx, manifest.Framework, "", name, namespace)
}

// Find service container with the give name, namespace and service name.
func (cli DockerEngine) FindService(ctx context.Context, name, namespace, service string) ([]container.Container, error) {
	if name == "" || namespace == "" {
		return nil, nil
	}
	return find(cli, ctx, manifest.Service, service, name, namespace)
}

func find(cli DockerEngine, ctx context.Context, category manifest.Category, service, name, namespace string) ([]container.Container, error) {
	args := filters.NewArgs()
	if category != "" {
		args.Add("label", CATEGORY_KEY+"="+string(category))
	}
	if service != "" {
		args.Add("label", SERVICE_NAME_KEY+"="+service)
	}
	if name != "" {
		args.Add("label", APP_NAME_KEY+"="+name)
	} else {
		args.Add("label", APP_NAME_KEY)
	}
	if namespace != "" {
		args.Add("label", APP_NAMESPACE_KEY+"="+namespace)
	} else {
		args.Add("label", APP_NAMESPACE_KEY)
	}

	options := types.ContainerListOptions{All: true, Filter: args}
	list, err := cli.ContainerList(ctx, options)
	if err != nil {
		return nil, err
	}

	containers := make([]container.Container, 0, len(list))
	for _, c := range list {
		cc, err := cli.Inspect(ctx, c.ID)
		if err != nil {
			return nil, err
		}
		containers = append(containers, cc)
	}
	return containers, nil
}

func (c *dockerContainer) ID() string {
	return c.ContainerJSON.ID
}

func (c *dockerContainer) Name() string {
	return c.Config.Labels[APP_NAME_KEY]
}

func (c *dockerContainer) Namespace() string {
	return c.Config.Labels[APP_NAMESPACE_KEY]
}

func (c *dockerContainer) Version() string {
	return c.Config.Labels[VERSION_KEY]
}

func (c *dockerContainer) Category() manifest.Category {
	return manifest.Category(c.Config.Labels[CATEGORY_KEY])
}

func (c *dockerContainer) PluginTag() string {
	return c.Config.Labels[PLUGIN_KEY]
}

func (c *dockerContainer) Flags() uint32 {
	flags, _ := strconv.ParseUint(c.Config.Labels[FLAGS_KEY], 10, 32)
	return uint32(flags)
}

func (c *dockerContainer) ServiceName() string {
	return c.Config.Labels[SERVICE_NAME_KEY]
}

func (c *dockerContainer) DependsOn() []string {
	depends := c.Config.Labels[SERVICE_DEPENDS_KEY]
	if depends != "" {
		return strings.Split(depends, ",")
	} else {
		return nil
	}
}

// Returns the host name of the container.
func (c *dockerContainer) Hostname() string {
	if c.Category().IsService() {
		return c.ServiceName() + "." + c.Name() + "-" + c.Namespace()
	} else {
		return c.Name() + "-" + c.Namespace()
	}
}

// Returns the fully qualified domain name of the container.
func (c *dockerContainer) FQDN() string {
	return c.Hostname() + "." + defaults.Domain()
}

// Returns the IP address of the container
func (c *dockerContainer) IP() (ip string) {
	if network := config.Get("network"); network != "" {
		if net := c.NetworkSettings.Networks[network]; net != nil {
			ip = net.IPAddress
		}
	}
	if ip == "" {
		ip = c.NetworkSettings.IPAddress
	}
	return ip
}

// Returns the container's operating system user that running the application.
func (c *dockerContainer) User() string {
	return c.Config.User
}

// Returns the application home directory within the container.
func (c *dockerContainer) Home() string {
	if home, ok := c.Config.Labels[APP_HOME_KEY]; ok {
		return home
	} else {
		return defaults.AppHome()
	}
}

func (c *dockerContainer) EnvDir() string {
	return c.Home() + "/.env"
}

func (c *dockerContainer) RepoDir() string {
	return c.Home() + "/repo"
}

func (c *dockerContainer) DeployDir() string {
	return c.Home() + "/deploy"
}

func (c *dockerContainer) DataDir() string {
	return c.Home() + "/data"
}

func (c *dockerContainer) LogDir() string {
	return c.Home() + "/logs"
}

func (c *dockerContainer) StartedAt() string {
	return c.State.StartedAt
}

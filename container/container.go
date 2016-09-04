package container

import (
	"fmt"
	"regexp"
	"strconv"
	"strings"

	"github.com/docker/engine-api/types"
	"github.com/docker/engine-api/types/filters"
	"golang.org/x/net/context"

	"github.com/cloudway/platform/config"
	"github.com/cloudway/platform/config/defaults"
	"github.com/cloudway/platform/pkg/manifest"
)

const (
	APP_NAME_KEY        = "com.cloudway.app.name"
	APP_NAMESPACE_KEY   = "com.cloudway.app.namespace"
	APP_HOME_KEY        = "com.cloudway.app.home"
	CATEGORY_KEY        = "com.cloudway.container.category"
	PLUGIN_KEY          = "com.cloudway.container.plugin"
	FLAGS_KEY           = "com.cloudway.container.flags"
	SERVICE_NAME_KEY    = "com.cloudway.service.name"
	SERVICE_DEPENDS_KEY = "com.cloudway.service.depends"
)

const (
	HotDeployable uint32 = 1 << iota
)

type Container struct {
	Name      string
	Namespace string

	DockerClient
	*types.ContainerJSON
}

var reNamePattern = regexp.MustCompile(`^((\*|[a-z][a-z_0-9]*)\.)?([a-z][a-z_0-9]*)-([a-z][a-z_0-9]*)$`)

func SplitNames(name string) (string, string, string) {
	m := reNamePattern.FindStringSubmatch(name)
	if len(m) != 0 {
		return m[2], m[3], m[4]
	} else {
		return "", "", ""
	}
}

// Returns an application container object constructed from the
// container id in the system.
func (cli DockerClient) Inspect(ctx context.Context, id string) (*Container, error) {
	info, err := cli.ContainerInspect(ctx, id)
	if err != nil {
		return nil, err
	}

	name := info.Config.Labels[APP_NAME_KEY]
	namespace := info.Config.Labels[APP_NAMESPACE_KEY]
	if name == "" || namespace == "" {
		return nil, fmt.Errorf("%s: Not a cloudway application container", id)
	}

	return &Container{
		Name:          name,
		Namespace:     namespace,
		DockerClient:  cli,
		ContainerJSON: &info,
	}, nil
}

// Find all containers with the given name and namespace.
func (cli DockerClient) FindAll(ctx context.Context, name, namespace string) ([]*Container, error) {
	cs, err := find(cli, ctx, "", "", name, namespace)
	if err != nil {
		return cs, err
	}

	// reorder the container list
	var cs2, i = make([]*Container, len(cs)), 0
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
func (cli DockerClient) FindApplications(ctx context.Context, name, namespace string) ([]*Container, error) {
	return find(cli, ctx, manifest.Framework, "", name, namespace)
}

// Find service container with the give name, namespace and service name.
func (cli DockerClient) FindService(ctx context.Context, name, namespace, service string) ([]*Container, error) {
	return find(cli, ctx, manifest.Service, service, name, namespace)
}

func find(cli DockerClient, ctx context.Context, category manifest.Category, service, name, namespace string) ([]*Container, error) {
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

	containers := make([]*Container, 0, len(list))
	for _, c := range list {
		cc, err := cli.Inspect(ctx, c.ID)
		if err != nil {
			return nil, err
		}
		containers = append(containers, cc)
	}
	return containers, nil
}

func (c *Container) Flags() uint32 {
	flags, _ := strconv.ParseUint(c.Config.Labels[FLAGS_KEY], 10, 32)
	return uint32(flags)
}

func (c *Container) Category() manifest.Category {
	return manifest.Category(c.Config.Labels[CATEGORY_KEY])
}

func (c *Container) PluginTag() string {
	return c.Config.Labels[PLUGIN_KEY]
}

func (c *Container) ServiceName() string {
	return c.Config.Labels[SERVICE_NAME_KEY]
}

func (c *Container) DependsOn() []string {
	depends := c.Config.Labels[SERVICE_DEPENDS_KEY]
	if depends != "" {
		return strings.Split(depends, ",")
	} else {
		return nil
	}
}

// Returns the host name of the container.
func (c *Container) Hostname() string {
	if c.Category().IsService() {
		return c.ServiceName() + "." + c.Name + "-" + c.Namespace
	} else {
		return c.Name + "-" + c.Namespace
	}
}

// Returns the fully qualified domain name of the container.
func (c *Container) FQDN() string {
	return c.Hostname() + "." + defaults.Domain()
}

// Returns the IP address of the container
func (c *Container) IP() (ip string) {
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
func (c *Container) User() string {
	return c.Config.User
}

// Returns the application home directory within the container.
func (c *Container) Home() string {
	if home, ok := c.Config.Labels[APP_HOME_KEY]; ok {
		return home
	} else {
		return defaults.AppHome()
	}
}

// Returns the env directory of the container.
func (c *Container) EnvDir() string {
	return c.Home() + "/.env"
}

func (c *Container) RepoDir() string {
	return c.Home() + "/repo"
}

func (c *Container) DeployDir() string {
	return c.Home() + "/deploy"
}

func (c *Container) DataDir() string {
	return c.Home() + "/data"
}

func (c *Container) LogDir() string {
	return c.Home() + "/logs"
}

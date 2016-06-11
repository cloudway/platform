package container

import (
    "fmt"
    "strings"

    "github.com/docker/engine-api/client"
    "github.com/docker/engine-api/types"
    "github.com/docker/engine-api/types/filters"
    "golang.org/x/net/context"

    "github.com/cloudway/platform/container/conf/defaults"
    "github.com/cloudway/platform/plugin"
)

const (
    APP_NAME_KEY        = "com.cloudway.app.name"
    APP_NAMESPACE_KEY   = "com.cloudway.app.namespace"
    APP_HOME_KEY        = "com.cloudway.app.home"
    CATEGORY_KEY        = "com.cloudway.container.category"
    SERVICE_NAME_KEY    = "com.cloudway.service.name"
    SERVICE_PLUGIN_KEY  = "com.cloudway.service.plugin"
    SERVICE_DEPENDS_KEY = "com.cloudway.service.depends"
)

var DEBUG bool

type Container struct {
    Name        string
    Namespace   string
    State       ContainerState

    *client.Client
    *types.ContainerJSON
}

var docker *client.Client

func docker_client() (cli *client.Client, err error) {
    if cli = docker; cli == nil {
        cli, err = client.NewEnvClient()
        docker = cli
    }
    return cli, err
}

// Returns an application container object constructed from the
// container id in the system.
func FromId(id string) (*Container, error) {
    cli, err := docker_client()
    if err != nil {
        return nil, err
    }
    return Inspect(cli, id)
}

func Inspect(cli *client.Client, id string) (*Container, error) {
    info, err := cli.ContainerInspect(context.Background(), id)
    if err != nil {
        return nil, err
    }

    name := info.Config.Labels[APP_NAME_KEY]
    namespace := info.Config.Labels[APP_NAMESPACE_KEY]
    if name == "" || namespace == "" {
        return nil, fmt.Errorf("%s: Not a cloudway application container", id)
    }

    return &Container{
        Name:           name,
        Namespace:      namespace,
        State:          ContainerState{info.State},
        Client:         cli,
        ContainerJSON:  &info,
    }, nil
}

// Returns a list of ids for every cloudway container in the system.
func IDs() ([]string, error) {
    cli, err := docker_client()
    if err != nil {
        return nil, err
    }
    return ids(cli)
}

func ids(cli *client.Client) (ids []string, err error) {
    args := filters.NewArgs()
    args.Add("label", APP_NAME_KEY)
    args.Add("label", APP_NAMESPACE_KEY)
    options := types.ContainerListOptions{All: true, Filter: args}

    containers, err := cli.ContainerList(context.Background(), options);
    if err == nil {
        ids = make([]string, len(containers))
        for i, c := range containers {
            ids[i] = c.ID
        }
    }
    return ids, err
}

// Returns a list of application container objects for every cloudway
// container in the system.
func All() ([]*Container, error) {
    cli, err := docker_client()
    if err != nil {
        return nil, err
    }

    ids, err := ids(cli)
    if err != nil {
        return nil, err
    }

    containers := make([]*Container, 0, len(ids))
    for _, id := range ids {
        c, err := Inspect(cli, id)
        if err != nil {
            return nil, err
        }
        containers = append(containers, c)
    }
    return containers, nil
}

// Find all containers with the given name and namespace.
func FindAll(name, namespace string) ([]*Container, error) {
    return find(name, namespace, filters.NewArgs())
}

// Find all application containers with the given name and namespace.
func FindApplications(name, namespace string) ([]*Container, error) {
    args := filters.NewArgs()
    args.Add("label", CATEGORY_KEY + "=" + string(plugin.Framework))
    return find(name, namespace, args)
}

// Find all service containers associated with the given name and namespace.
func FindServices(name, namespace string) ([]*Container, error) {
    args := filters.NewArgs()
    args.Add("label", CATEGORY_KEY + "=" + string(plugin.Service))
    return find(name, namespace, args)
}

// Find service container with the give name, namespace and service name.
func FindService(name, namespace, service string) ([]*Container, error) {
    args := filters.NewArgs()
    args.Add("label", CATEGORY_KEY + "=" + string(plugin.Service))
    args.Add("label", SERVICE_NAME_KEY + "=" + service)
    return find(name, namespace, args)
}

func find(name, namespace string, args filters.Args) ([]*Container, error) {
    cli, err := docker_client()
    if err != nil {
        return nil, err
    }

    args.Add("label", APP_NAME_KEY + "=" + name)
    args.Add("label", APP_NAMESPACE_KEY + "=" + namespace)
    options := types.ContainerListOptions{All: true, Filter: args}

    list, err := cli.ContainerList(context.Background(), options)
    if err != nil {
        return nil, err
    }

    containers := make([]*Container, 0, len(list))
    for _, c := range list {
        cc, err := Inspect(cli, c.ID)
        if err != nil {
            return nil, err
        }
        containers = append(containers, cc)
    }
    return containers, nil
}

func (c *Container) Category() plugin.Category {
     return plugin.Category(c.Config.Labels[CATEGORY_KEY])
}

func (c *Container) ServiceName() string {
    return c.Config.Labels[SERVICE_NAME_KEY]
}

func (c *Container) ServicePlugin() string {
    return c.Config.Labels[SERVICE_PLUGIN_KEY]
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
    if c.Category() == plugin.Service {
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
func (c *Container) IP() string {
    return c.NetworkSettings.IPAddress
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

func (c *Container) DataDir() string {
    return c.Home() + "/data"
}

func (c *Container) LogDir() string {
    return c.Home() + "/logs"
}

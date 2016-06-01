package container

import (
    "fmt"

    "github.com/docker/engine-api/client"
    "github.com/docker/engine-api/types"
    "github.com/docker/engine-api/types/filters"
    "golang.org/x/net/context"

    "github.com/cloudway/platform/container/conf/defaults"
)

const (
    _APP_NAME_KEY       = "com.cloudway.app.name"
    _APP_NAMESPACE_KEY  = "com.cloudway.app.namespace"
    _APP_HOME_KEY       = "com.cloudway.app.home"
    _APP_CAPACITY_KEY   = "com.cloudway.app.capacity"
)

var DEBUG bool

type Container struct {
    *client.Client

    ID          string
    Name        string
    Namespace   string

    info        types.ContainerJSON
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
    return fromId(cli, id)
}

func fromId(cli *client.Client, id string) (*Container, error) {
    info, err := cli.ContainerInspect(context.Background(), id)
    if err != nil {
        return nil, err
    }

    name := info.Config.Labels[_APP_NAME_KEY]
    namespace := info.Config.Labels[_APP_NAMESPACE_KEY]
    if name == "" || namespace == "" {
        return nil, fmt.Errorf("%s: Not a cloudway application container", id)
    }

    return &Container{
        ID:         info.ID,
        Name:       name,
        Namespace:  namespace,
        Client:     cli,
        info:       info,
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
    args.Add("label", _APP_NAME_KEY)
    args.Add("label", _APP_NAMESPACE_KEY)
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
func All() (containers []*Container, err error) {
    cli, err := docker_client()
    if err != nil {
        return nil, err
    }

    ids, err := ids(cli)
    if err == nil {
        containers = make([]*Container, len(ids))
        for i, id := range ids {
            c, err := fromId(cli, id)
            if err != nil {
                break
            }
            containers[i] = c
        }
    }
    return containers, err
}

// Find all application containers with the given name and namespace.
func Find(name, namespace string) (containers []*Container, err error) {
    cli, err := docker_client()
    if err != nil {
        return nil, err
    }

    args := filters.NewArgs()
    args.Add("label", _APP_NAME_KEY + "=" + name)
    args.Add("label", _APP_NAMESPACE_KEY + "=" + namespace)
    options := types.ContainerListOptions{All: true, Filter: args}

    list, err := cli.ContainerList(context.Background(), options);
    if err == nil {
        containers = make([]*Container, len(list))
        for i, c := range list {
            cc, err := fromId(cli, c.ID)
            if err != nil {
                break;
            }
            containers[i] = cc
        }
    }
    return containers, err
}

// Returns the fully qualified domain name of the container.
func (c *Container) FQDN() string {
    return c.Name + "-" + c.Namespace + "." + defaults.Domain()
}

// Get the current application state.
func (c *Container) State() ContainerState {
    return ContainerState{c.info.State}
}

// Get the container capacity.
func (c *Container) Capacity() string {
    if capacity, ok := c.info.Config.Labels[_APP_CAPACITY_KEY]; ok {
        return capacity
    } else {
        return defaults.AppCapacity()
    }
}

// Returns the container's operating system user that running the application.
func (c *Container) User() string {
    return c.info.Config.User
}

// Returns the application home directory within the container.
func (c *Container) Home() string {
    if home, ok := c.info.Config.Labels[_APP_HOME_KEY]; ok {
        return home
    } else {
        return defaults.AppHome()
    }
}

// Returns the env directory of the container.
func (c *Container) EnvDir() string {
    return c.Home() + "/.env"
}

func (c *Container) AppDir() string {
    return c.Home() + "/app"
}

func (c *Container) RepoDir() string {
    return c.AppDir() + "/repo"
}

func (c *Container) DataDir() string {
    return c.AppDir() + "/data"
}

func (c *Container) LogDir() string {
    return c.AppDir() + "/logs"
}

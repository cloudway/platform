package container

import "golang.org/x/net/context"

const _WAIT_SECONDS = 1

// Start the application container.
func (c *Container) Start() error {
    cli, err := docker_client()
    if err == nil {
        err = cli.ContainerStart(context.Background(), c.ID, "")
    }
    return err
}

// Stop the application container.
func (c *Container) Stop() error {
    cli, err := docker_client()
    if err == nil {
        err = cli.ContainerStop(context.Background(), c.ID, _WAIT_SECONDS)
    }
    return err
}

// Restart the application container.
func (c *Container) Restart() error {
    cli, err := docker_client()
    if err == nil {
        err = cli.ContainerRestart(context.Background(), c.ID, _WAIT_SECONDS)
    }
    return err
}

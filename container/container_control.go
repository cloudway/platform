package container

import "golang.org/x/net/context"

const _WAIT_SECONDS = 60

// Start the application container.
func (c *Container) Start() error {
    return c.ContainerStart(context.Background(), c.ID, "")
}

// Stop the application container.
func (c *Container) Stop() error {
    return c.ContainerStop(context.Background(), c.ID, _WAIT_SECONDS)
}

// Restart the application container.
func (c *Container) Restart() error {
    return c.ContainerRestart(context.Background(), c.ID, _WAIT_SECONDS)
}

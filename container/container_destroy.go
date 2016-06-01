package container

import (
    "golang.org/x/net/context"
    "github.com/docker/engine-api/types"
    "fmt"
)

// Destroy the application container.
func (c *Container) Destroy() error {
    image := c.info.Config.Image

    // remove the container, force kill if it's running
    options := types.ContainerRemoveOptions{Force: true, RemoveVolumes: true}
    err := c.ContainerRemove(context.Background(), c.ID, options)
    if err != nil {
        return err
    }
    if (DEBUG) { fmt.Printf("Removed container %s\n", c.ID) }

    // remove associated image
    if image != "" {
        options := types.ImageRemoveOptions{Force: true, PruneChildren: true}
        c.ImageRemove(context.Background(), image, options)
        if (DEBUG) { fmt.Printf("Removed image %s\n", image) }
    }

    return nil
}

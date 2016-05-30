package container

import (
    "golang.org/x/net/context"
    "github.com/docker/engine-api/types"
    "fmt"
)

// Destroy the application container.
func (c *Container) Destroy() error {
    cli, err := docker_client()
    if err != nil {
        return err
    }

    imageId := c.info.Config.Labels[_IMAGE_ID_KEY]

    // remove the container, force kill if it's running
    options := types.ContainerRemoveOptions{Force: true, RemoveVolumes: true}
    err = cli.ContainerRemove(context.Background(), c.ID, options)
    if err != nil {
        return err
    }
    if (DEBUG) { fmt.Printf("Removed container %s\n", c.ID) }

    // remove associated image
    if imageId != "" {
        options := types.ImageRemoveOptions{Force: true, PruneChildren: true}
        cli.ImageRemove(context.Background(), imageId, options)
        if (DEBUG) { fmt.Printf("Removed image %s\n", imageId) }
    }

    return nil
}

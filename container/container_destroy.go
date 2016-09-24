package container

import (
	"context"

	"github.com/Sirupsen/logrus"
	"github.com/docker/engine-api/types"
)

// Destroy the application container.
func (c *Container) Destroy(ctx context.Context) error {
	image := c.Config.Image

	// remove the container, force kill if it's running
	options := types.ContainerRemoveOptions{Force: true, RemoveVolumes: true}
	err := c.ContainerRemove(ctx, c.ID, options)
	if err != nil {
		return err
	}
	logrus.Debugf("Removed container %s", c.ID)

	// remove associated image
	if image != "" {
		options := types.ImageRemoveOptions{Force: false, PruneChildren: true}
		c.ImageRemove(ctx, image, options)
		logrus.Debugf("Removed image %s", image)
	}

	return nil
}

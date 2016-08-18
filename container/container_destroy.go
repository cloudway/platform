package container

import (
	"github.com/Sirupsen/logrus"
	"github.com/docker/engine-api/types"
	"golang.org/x/net/context"
)

// Destroy the application container.
func (c *Container) Destroy() error {
	image := c.Config.Image

	// remove the container, force kill if it's running
	options := types.ContainerRemoveOptions{Force: true, RemoveVolumes: true}
	err := c.ContainerRemove(context.Background(), c.ID, options)
	if err != nil {
		return err
	}
	logrus.Debugf("Removed container %s", c.ID)

	// remove associated image
	if image != "" {
		options := types.ImageRemoveOptions{Force: true, PruneChildren: true}
		c.ImageRemove(context.Background(), image, options)
		logrus.Debugf("Removed image %s", image)
	}

	return nil
}

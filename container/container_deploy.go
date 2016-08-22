package container

import (
	"archive/tar"
	"io"

	"golang.org/x/net/context"

	"github.com/cloudway/platform/pkg/archive"
	"github.com/docker/engine-api/types"
)

func (c *Container) Deploy(ctx context.Context, path string) error {
	// Create context archive containing the repo archive
	r, w := io.Pipe()
	go func() {
		tw := tar.NewWriter(w)
		err := archive.CopyFileTree(tw, "", path, nil, false)
		tw.Close()
		w.CloseWithError(err)
	}()

	// Copy file to container
	err := c.CopyToContainer(ctx, c.ID, c.DeployDir(), r, types.CopyToContainerOptions{})
	if err != nil {
		return err
	}

	// Send signal to container to complete the deployment
	c.ContainerKill(ctx, c.ID, "SIGHUP")
	return nil
}

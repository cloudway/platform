package container

import (
    "io"
    "archive/tar"
    "github.com/docker/distribution/context"
    "github.com/docker/engine-api/types"
    "github.com/cloudway/platform/pkg/archive"
)

func (c *Container) Deploy(path string) error {
    // Create context archive containing the repo archive
    r, w := io.Pipe()
    go func() {
        tw := tar.NewWriter(w)
        err := archive.CopyFileTree(tw, "", path, false)
        tw.Close()
        w.CloseWithError(err)
    }()

    // Copy file to container
    err := c.CopyToContainer(context.Background(), c.ID, c.DeployDir(), r, types.CopyToContainerOptions{})
    if err != nil {
        return err
    }

    // Send signal to container to complete the deployment
    c.ContainerKill(context.Background(), c.ID, "SIGHUP")
    return nil
}

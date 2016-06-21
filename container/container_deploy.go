package container

import (
    "io"
    "time"
    "archive/tar"
    "github.com/docker/distribution/context"
    "github.com/docker/engine-api/types"
)

func (c *Container) Deploy(ar io.Reader, name string, size int64) error {
    // Create context archive containing the repo archive
    r, w := io.Pipe()
    go func() {
        err := writeArchive(w, ar, name, size)
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

func writeArchive(w io.Writer, r io.Reader, name string, size int64) error {
    tw := tar.NewWriter(w)
    err := tw.WriteHeader(&tar.Header{
        Name:    name,
        Size:    size,
        Mode:    0666, // allow non-root user to read/write file
        ModTime: time.Now(),
    })
    if err != nil {
        return err
    }
    _, err = io.Copy(w, r)
    return err
}

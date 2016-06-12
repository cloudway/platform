package container

import (
    "os"
    "bytes"
    "archive/tar"
    "golang.org/x/net/context"
    "github.com/Sirupsen/logrus"
    "github.com/docker/engine-api/types"
)

const _WAIT_SECONDS = 60

// Start the application container.
func (c *Container) Start() error {
    err := c.ContainerStart(context.Background(), c.ID, "")
    if err != nil {
        return err
    }
    return startContainer(c)
}

// Restart the application container.
func (c *Container) Restart() error {
    err := c.ContainerRestart(context.Background(), c.ID, _WAIT_SECONDS)
    if err != nil {
        return err
    }
    return startContainer(c)
}

// Stop the application container.
func (c *Container) Stop() error {
    return c.ContainerStop(context.Background(), c.ID, _WAIT_SECONDS)
}

func startContainer(c *Container) error {
    err := c.Exec("", nil, os.Stdout, os.Stderr, "/usr/bin/cwctl", "start")
    if err != nil {
        return err
    }

    info, err := c.GetInfo()
    if err != nil {
        return err
    }

    return distributeEnv(c, info.Env)
}

func distributeEnv(c *Container, env map[string]string) error {
    if !c.Category().IsService() || len(env) == 0 {
        return nil
    }

    // Create an archive that contains all exported environment files
    envfile := createEnvFile(env)

    // Write environments to all containers in the application
    cs, err := FindAll(c.Name, c.Namespace)
    if err != nil {
        return err
    }

    ctx := context.Background()
    opt := types.CopyToContainerOptions{}
    for _, cc := range cs {
        if cc.ID != c.ID {
            err := cc.CopyToContainer(ctx, cc.ID, cc.EnvDir(), bytes.NewReader(envfile), opt)
            if err != nil {
                logrus.Error(err)
            }
        }
    }

    return nil
}

func createEnvFile(env map[string]string) []byte {
    buf := &bytes.Buffer{}
    tw := tar.NewWriter(buf)

    for name, value := range env {
        hdr := tar.Header{
            Name: name,
            Size: int64(len(value)),
            Mode: 0644,
        }
        tw.WriteHeader(&hdr)
        tw.Write([]byte(value))
    }

    tw.Close()
    return buf.Bytes()
}

package container

import (
	"archive/tar"
	"bytes"
	"time"

	"github.com/Sirupsen/logrus"
	"github.com/cloudway/platform/pkg/serverlog"
	"github.com/docker/engine-api/types"
	"golang.org/x/net/context"
)

var waitTimeout = time.Second * 60

// Start the application container.
func (c *Container) Start(ctx context.Context, log *serverlog.ServerLog) error {
	err := c.ContainerStart(ctx, c.ID, types.ContainerStartOptions{})
	if err != nil {
		return err
	}
	return startSandbox(ctx, c, log)
}

// Restart the application container.
func (c *Container) Restart(ctx context.Context, log *serverlog.ServerLog) error {
	err := c.ContainerRestart(ctx, c.ID, &waitTimeout)
	if err != nil {
		return err
	}
	return startSandbox(ctx, c, log)
}

// Stop the application container.
func (c *Container) Stop(ctx context.Context) error {
	return c.ContainerStop(ctx, c.ID, &waitTimeout)
}

func startSandbox(ctx context.Context, c *Container, log *serverlog.ServerLog) error {
	err := c.Exec(ctx, "", nil, log.Stdout(), log.Stderr(), "/usr/bin/cwctl", "start")
	if err != nil {
		return err
	}

	info, err := c.GetInfo(ctx, "env")
	if err != nil {
		return err
	}

	return distributeEnv(ctx, c, info.Env)
}

func distributeEnv(ctx context.Context, c *Container, env map[string]string) error {
	if !c.Category().IsService() || len(env) == 0 {
		return nil
	}

	// Create an archive that contains all exported environment files
	envfile := createEnvFile(env)

	// Write environments to all containers in the application
	cs, err := c.FindAll(ctx, c.Name, c.Namespace)
	if err != nil {
		return err
	}

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

package docker

import (
	"context"
	"io"

	"github.com/docker/engine-api/types"
)

func (c *dockerContainer) CopyFrom(ctx context.Context, path string) (io.ReadCloser, error) {
	content, _, err := c.CopyFromContainer(ctx, c.ID(), path)
	return content, err
}

func (c *dockerContainer) CopyTo(ctx context.Context, path string, content io.Reader) error {
	opts := types.CopyToContainerOptions{AllowOverwriteDirWithFile: true}
	return c.CopyToContainer(ctx, c.ID(), path, content, opts)
}

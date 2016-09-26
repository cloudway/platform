package docker

import (
	"bytes"
	"context"
	"encoding/json"

	"github.com/cloudway/platform/pkg/manifest"
)

// Get application information from container.
func (c *dockerContainer) GetInfo(ctx context.Context, options ...string) (*manifest.SandboxInfo, error) {
	var args = []string{"/usr/bin/cwctl", "info", "--ip", c.IP()}
	for _, opt := range options {
		args = append(args, "--"+opt)
	}

	var buf bytes.Buffer
	err := c.ExecE(ctx, "root", nil, &buf, args...)
	if err != nil {
		return nil, err
	}

	var info manifest.SandboxInfo
	if err = json.NewDecoder(&buf).Decode(&info); err != nil {
		return nil, err
	} else {
		return &info, err
	}
}

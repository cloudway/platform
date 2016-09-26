package docker

import (
	"archive/tar"
	"bytes"
	"context"
	"errors"
	"io/ioutil"
	"regexp"
	"strconv"
	"strings"

	"github.com/cloudway/platform/pkg/manifest"
)

// Adds the variable to the environment with the value, if name does not
// exists. If name does exist in the environment, then its value is changed
// to value.
func (c *dockerContainer) Setenv(ctx context.Context, name, value string) error {
	content := []byte(value)

	// Make an archive containing the environmnet file
	buf := &bytes.Buffer{}
	tw := tar.NewWriter(buf)
	tw.WriteHeader(&tar.Header{
		Name: name,
		Mode: 0644,
		Size: int64(len(content)),
	})
	tw.Write(content)
	tw.Close()

	// Copy the archive to the container at specified path
	return c.CopyTo(ctx, c.EnvDir(), buf)
}

// Get an environment variable value.
func (c *dockerContainer) Getenv(ctx context.Context, name string) (string, error) {
	// Copy the archive from the container that contains the environment file
	path := c.EnvDir() + "/" + name
	r, err := c.CopyFrom(ctx, path)
	if err != nil {
		return "", err
	}
	defer r.Close()

	// Read archive to get environment value
	var content []byte
	tr := tar.NewReader(r)
	if _, err = tr.Next(); err != nil {
		return "", err
	}
	if content, err = ioutil.ReadAll(tr); err != nil {
		return "", err
	}
	return strings.TrimRight(string(content), "\r\n"), nil
}

func (c *dockerContainer) ActiveState(ctx context.Context) manifest.ActiveState {
	// Get active state from running processes
	if c.State.Running {
		state, err := c.activeStateFromRunningProcess(ctx)
		if err == nil {
			return state
		}
	} else {
		return manifest.StateStopped
	}

	// Fallback to get active state from state file
	str, err := c.Getenv(ctx, ".state")
	if err != nil {
		return manifest.StateUnknown
	}
	i, err := strconv.Atoi(str)
	if err != nil {
		return manifest.StateUnknown
	}
	return manifest.ActiveState(i)
}

var statePattern = regexp.MustCompile(`^/usr/bin/cwctl \[([0-9])\]`)

func (c *dockerContainer) activeStateFromRunningProcess(ctx context.Context) (manifest.ActiveState, error) {
	ps, err := c.ContainerTop(ctx, c.ID(), []string{})
	if err != nil {
		return manifest.StateUnknown, err
	}

	var cmdIndex = -1
	for i, t := range ps.Titles {
		if strings.ToUpper(t) == "COMMAND" {
			cmdIndex = i
			break
		}
	}
	if cmdIndex == -1 {
		return manifest.StateUnknown, errors.New("no command column in process list")
	}

	for _, p := range ps.Processes {
		command := p[cmdIndex]
		if m := statePattern.FindStringSubmatch(command); m != nil {
			state, err := strconv.Atoi(command)
			return manifest.ActiveState(state), err
		}
	}
	return manifest.StateUnknown, errors.New("sandbox process not found")
}

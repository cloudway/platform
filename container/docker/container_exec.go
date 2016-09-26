package docker

import (
	"bytes"
	"context"
	"io"
	"strings"

	"github.com/Sirupsen/logrus"
	"github.com/cloudway/platform/container"
	"github.com/cloudway/platform/pkg/stdcopy"
	"github.com/docker/engine-api/types"
)

// Execute command in application container.
func (c *dockerContainer) Exec(ctx context.Context, user string, stdin io.Reader, stdout, stderr io.Writer, cmd ...string) error {
	// FIXME: Output may be closed if no stdin attached at sometimes.
	// To workaround this problem always attach the stdin. This problem
	// just occurres in docker swarm cluster, so it may be a docker bug.
	if stdin == nil {
		stdin = bytes.NewReader(nil)
	}

	execConfig := types.ExecConfig{
		User:         user,
		Tty:          false,
		AttachStdin:  stdin != nil,
		AttachStdout: true,
		AttachStderr: true,
		Cmd:          cmd,
	}

	execResp, err := c.ContainerExecCreate(ctx, c.ID(), execConfig)
	if err != nil {
		return err
	}
	execId := execResp.ID

	resp, err := c.ContainerExecAttach(ctx, execId, execConfig)
	if err != nil {
		return err
	}
	defer resp.Close()

	err = pumpStreams(ctx, stdin, stdout, stderr, resp)
	if err != nil {
		return err
	}

	inspectResp, err := c.ContainerExecInspect(ctx, execId)
	if err != nil {
		return err
	} else if inspectResp.ExitCode != 0 {
		return container.StatusError{
			Command: cmd,
			Code:    inspectResp.ExitCode,
		}
	} else {
		return nil
	}
}

func pumpStreams(ctx context.Context, stdin io.Reader, stdout, stderr io.Writer, resp types.HijackedResponse) error {
	var err error

	receiveStdout := make(chan error, 1)
	if stdout != nil || stderr != nil {
		go func() {
			_, err = stdcopy.Copy(stdout, stderr, nil, resp.Reader)
			logrus.Debugf("[hijack] End of stdout")
			receiveStdout <- err
		}()
	}

	stdinDone := make(chan struct{})
	go func() {
		if stdin != nil {
			io.Copy(resp.Conn, stdin)
			logrus.Debugf("[hijack] End of stdin")
		}
		resp.CloseWrite()
		close(stdinDone)
	}()

	select {
	case err := <-receiveStdout:
		if err != nil {
			logrus.WithError(err).Debugf("Error receive stdout")
			return err
		}
	case <-stdinDone:
		if stdout != nil || stderr != nil {
			select {
			case err := <-receiveStdout:
				if err != nil {
					logrus.WithError(err).Debugf("Error receive stdout")
					return err
				}
			case <-ctx.Done():
			}
		}
	case <-ctx.Done():
	}

	return nil
}

// Execute the command and accumulate error messages from standard error of
// the command.
func (c *dockerContainer) ExecE(ctx context.Context, user string, in io.Reader, out io.Writer, cmd ...string) error {
	var errbuf bytes.Buffer
	err := c.Exec(ctx, user, in, out, &errbuf, cmd...)
	if se, ok := err.(container.StatusError); ok && se.Message == "" {
		se.Message = chomp(&errbuf)
	}
	return err
}

// Silently execute the command and accumulate error messages from standard
// error of the command.
//
// Equivalent to: ExecE(user, nil, nil, cmd...)
func (c *dockerContainer) ExecQ(ctx context.Context, user string, cmd ...string) error {
	return c.ExecE(ctx, user, nil, nil, cmd...)
}

// Performs the expansion by executing command and return the contents
// as the standard output of the command, with any trailing newlines
// deleted.
func (c *dockerContainer) Subst(ctx context.Context, user string, in io.Reader, cmd ...string) (string, error) {
	var outbuf, errbuf bytes.Buffer
	err := c.Exec(ctx, user, in, &outbuf, &errbuf, cmd...)
	if se, ok := err.(container.StatusError); ok && se.Message == "" {
		se.Message = chomp(&errbuf)
	}
	return chomp(&outbuf), err
}

func chomp(b *bytes.Buffer) string {
	return strings.TrimRight(b.String(), "\r\n")
}

func (c *dockerContainer) Processes(ctx context.Context) (*container.ProcessList, error) {
	if top, err := c.ContainerTop(ctx, c.ID(), nil); err == nil {
		return &container.ProcessList{
			Processes: top.Processes,
			Headers:   top.Titles,
		}, nil
	} else {
		return nil, err
	}
}

func (c *dockerContainer) Stats(ctx context.Context, stream bool) (io.ReadCloser, error) {
	return c.ContainerStats(ctx, c.ID(), stream)
}

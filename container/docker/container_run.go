package docker

import (
	"context"
	"io"

	"github.com/Sirupsen/logrus"
	"github.com/cloudway/platform/container"
	"github.com/docker/engine-api/types"
)

func (c *dockerContainer) Run(ctx context.Context, cmd *container.RunCmd) (err error) {
	execConfig := types.ExecConfig{
		Tty:          true,
		AttachStdin:  true,
		AttachStdout: true,
		AttachStderr: true,
		Cmd:          cmd.Cmd,
	}

	if cmd.CopyInput == nil {
		cmd.CopyInput = io.Copy
	}
	if cmd.CopyOutput == nil {
		cmd.CopyOutput = io.Copy
	}

	execResp, err := c.ContainerExecCreate(ctx, c.ID(), execConfig)
	if err != nil {
		return
	}
	cmd.ExecID = execResp.ID

	if cmd.BeforeStart != nil {
		if err = cmd.BeforeStart(cmd); err != nil {
			return
		}
	}

	resp, err := c.ContainerExecAttach(ctx, cmd.ExecID, execConfig)
	if err != nil {
		return
	}

	if cmd.Size != nil {
		c.ExecResize(ctx, cmd.ExecID, *cmd.Size)
	}

	go func() {
		defer resp.Close()

		// pipe stream to container and vice-versa
		go func() {
			cmd.CopyInput(resp.Conn, cmd.Stdin)
			resp.CloseWrite()
			logrus.Debug("[hijack] End of stdin")
		}()

		cmd.CopyOutput(cmd.Stdout, resp.Reader)
		logrus.Debug("[hijack] End of stdout")

		// send exit code to caller
		inspectResp, err := c.ContainerExecInspect(ctx, cmd.ExecID)
		if err != nil {
			logrus.WithError(err).Error("Could not inspect exec")
			cmd.ExitCode = 127
		} else {
			cmd.ExitCode = inspectResp.ExitCode
		}

		if cmd.OnExit != nil {
			cmd.OnExit(cmd)
		}
	}()

	return
}

func (cli DockerEngine) ExecResize(ctx context.Context, execID string, resize container.TtySize) error {
	opt := types.ResizeOptions{Width: resize.Width, Height: resize.Height}
	return cli.ContainerExecResize(ctx, execID, opt)
}

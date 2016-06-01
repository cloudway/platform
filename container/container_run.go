package container

import (
    "io"
    "sync"
    "golang.org/x/net/context"
    "github.com/docker/engine-api/types"
)

// Run interactive command in a running container.
func (c *Container) Run(user string, cmd ...string) error {
    tty := NewTty()

    execConfig := types.ExecConfig{
        User:           user,
        Tty:            tty.isTtyMode(),
        AttachStdin:    true,
        AttachStdout:   true,
        AttachStderr:   true,
        Cmd:            cmd,
    }

    ctx := context.Background()

    execResp, err := c.ContainerExecCreate(ctx, c.ID, execConfig)
    if err != nil {
        return err
    }
    execId := execResp.ID

    resp, err := c.ContainerExecAttach(ctx, execId, execConfig)
    if err != nil {
        return err
    }
    defer resp.Close()

    if execConfig.Tty && tty.isTerminalIn {
        tty.monitorResize(func (w, h int) error {
            options := types.ResizeOptions{Width: w, Height: h}
            return c.ContainerExecResize(ctx, execId, options)
        });
    }

    err = tty.pumpStreams(ctx, execConfig.Tty, resp)
    if err != nil {
        return err
    }

    inspectResp, err := c.ContainerExecInspect(ctx, execId)
    if err != nil {
        return err
    } else if inspectResp.ExitCode != 0 {
        return StatusError{Code: inspectResp.ExitCode}
    } else {
        return nil
    }
}

func (t *Tty) pumpStreams(ctx context.Context, tty bool, resp types.HijackedResponse) error {
    var err error

    var restoreOnce sync.Once
    if tty {
        if err = t.makeRaw(); err != nil {
            return err
        }
        defer func() {
            restoreOnce.Do(t.restore)
        }()
    }

    receiveStdout := make(chan error, 1)
    go func() {
        if tty {
            // When TTY is ON, use regular copy
            _, err = io.Copy(t.out, resp.Reader)
            // we should restore the terminal as soon as possible once
            // connection end so any following print messages will be
            // in normal type.
            restoreOnce.Do(t.restore)
        } else {
            _, err = stdCopy(t.out, t.err, resp.Reader)
        }
        receiveStdout <- err
    }()

    stdinDone := make(chan struct{})
    go func() {
        io.Copy(resp.Conn, t.in)
        // we should restore the terminal as soon as possible once connection
        // end so any following print messages will be in normal type.
        if tty {
            restoreOnce.Do(t.restore)
        }
        resp.CloseWrite()
        close(stdinDone)
    }()

    select {
    case err := <-receiveStdout:
        if err != nil {
            return err
        }
    case <-stdinDone:
        select {
        case err := <-receiveStdout:
            if err != nil {
                return err
            }
        case <-ctx.Done():
        }
    case <-ctx.Done():
    }

    return nil
}

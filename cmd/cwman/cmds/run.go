package cmds

import (
    "github.com/cloudway/platform/pkg/mflag"
    "github.com/cloudway/platform/container"
)

func (cli *CWMan) CmdRun(args ...string) error {
    cmd := cli.Subcmd("run", "CONTAINER [COMMAND [ARG...]]")
    user := cmd.String([]string{"u", "-user"}, "", "Username")
    cmd.Require(mflag.Min, 1)
    cmd.ParseFlags(args, true)

    return runContainerAction(cmd.Arg(0), func (c *container.Container) error {
        cargs := cmd.Args()[1:]
        if len(cargs) == 0 {
            cargs = []string{"/usr/bin/cwsh"}
        }

        err := c.Run(*user, cargs...)
        if _, ok := err.(container.StatusError); !ok {
            return err
        } else {
            return nil
        }
    })
}
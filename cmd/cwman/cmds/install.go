package cmds

import (
    "github.com/cloudway/platform/pkg/mflag"
    "github.com/cloudway/platform/container"
)

func (cli *CWMan) CmdInstall(args ...string) error {
    cmd := cli.Subcmd("install", "CONTAINER PATH")
    cmd.Require(mflag.Exact, 2)
    cmd.ParseFlags(args, true)

    return cli.runContainerAction(cmd.Arg(0), func(c *container.Container) error {
        return c.Install(cmd.Arg(1))
    })
}

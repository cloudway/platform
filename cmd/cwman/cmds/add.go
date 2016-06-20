package cmds

import (
    "github.com/cloudway/platform/pkg/mflag"
    "github.com/cloudway/platform/container"
    "github.com/cloudway/platform/hub"
)

func (cli *CWMan) CmdAdd(args ...string) error {
    cmd := cli.Subcmd("add", "CONTAINER PATH")
    cmd.Require(mflag.Exact, 2)
    cmd.ParseFlags(args, true)

    hub, err := hub.New()
    if err != nil {
        return err
    }
    pluginPath, err := hub.GetPluginPath(cmd.Arg(1))
    if err != nil {
        return err
    }

    return cli.runContainerAction(cmd.Arg(0), func(c *container.Container) error {
        return c.Install(pluginPath)
    })
}

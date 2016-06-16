package cmds

import (
    "github.com/cloudway/platform/pkg/mflag"
    "github.com/cloudway/platform/container"
    "fmt"
)

func (cli *CWMan) CmdEnv(args ...string) error {
    cmd := cli.Subcmd("env", "CONTAINER [CONTAINER...]")
    cmd.Require(mflag.Min, 1)
    cmd.ParseFlags(args, true)

    return cli.runContainerAction(cmd.Arg(0), func(c *container.Container) error {
        info, err := c.GetInfo()
        if err != nil {
            return err
        }

        fmt.Printf("%s:\n", c.FQDN())
        for k, v := range info.Env {
            fmt.Printf("%s=%q\n", k, v)
        }
        fmt.Println()

        return nil
    })
}

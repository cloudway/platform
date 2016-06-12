package cmds

import (
    flag "github.com/cloudway/platform/pkg/mflag"
    "github.com/cloudway/platform/container"
)

func (cli *CWMan) CmdStart(args ...string) error {
    cmd := cli.Subcmd("start", "CONTAINER [CONTAINER...]", "all")
    cmd.Require(flag.Min, 1)
    cmd.ParseFlags(args, true)
    return runControlCmd(cmd.Args(), (*container.Container).Start)
}

func (cli *CWMan) CmdStop(args ...string) error {
    cmd := cli.Subcmd("stop", "CONTAINER [CONTAINER...]", "all")
    cmd.Require(flag.Min, 1)
    cmd.ParseFlags(args, true)
    return runControlCmd(cmd.Args(), (*container.Container).Stop)
}

func (cli *CWMan) CmdRestart(args ...string) error {
    cmd := cli.Subcmd("restart", "CONTAINER [CONTAINER...]", "all")
    cmd.Require(flag.Min, 1)
    cmd.ParseFlags(args, true)
    return runControlCmd(cmd.Args(), (*container.Container).Restart)
}

func (cli *CWMan) CmdDestroy(args ...string) error {
    cmd := cli.Subcmd("destroy", "CONTAINER [CONTAINER...]", "all")
    cmd.Require(flag.Min, 1)
    cmd.ParseFlags(args, true)
    return runControlCmd(cmd.Args(), (*container.Container).Destroy)
}

func runControlCmd(args []string, action func(*container.Container) error) error {
    if len(args) == 1 && args[0] == "all" {
        containers, err := container.All()
        if err != nil {
            return err
        }
        for _, c := range containers {
            if err = action(c); err != nil {
                return err
            }
        }
    } else {
        for _, id := range args {
            if err := runContainerAction(id, action); err != nil {
                return err
            }
        }
    }
    return nil
}

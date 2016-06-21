package cmds

import (
    "os"
    "github.com/Sirupsen/logrus"
    flag "github.com/cloudway/platform/pkg/mflag"
    "github.com/cloudway/platform/container"
)

func (cli *CWMan) CmdStart(args ...string) error {
    cmd := cli.Subcmd("start", "CONTAINER [CONTAINER...]", "all")
    cmd.Require(flag.Min, 1)
    cmd.ParseFlags(args, true)
    return runControlCmd(cli, cmd.Args(), (*container.Container).Start)
}

func (cli *CWMan) CmdStop(args ...string) error {
    cmd := cli.Subcmd("stop", "CONTAINER [CONTAINER...]", "all")
    cmd.Require(flag.Min, 1)
    cmd.ParseFlags(args, true)
    return runControlCmd(cli, cmd.Args(), (*container.Container).Stop)
}

func (cli *CWMan) CmdRestart(args ...string) error {
    cmd := cli.Subcmd("restart", "CONTAINER [CONTAINER...]", "all")
    cmd.Require(flag.Min, 1)
    cmd.ParseFlags(args, true)
    return runControlCmd(cli, cmd.Args(), (*container.Container).Restart)
}

func (cli *CWMan) CmdStatus(args ...string) error {
    cmd := cli.Subcmd("status", "CONTAINER [CONTAINER...]")
    cmd.Require(flag.Min, 1)
    cmd.ParseFlags(args, true)

    return runControlCmd(cli, cmd.Args(), func(c *container.Container) error {
        err := c.Exec("", nil, os.Stdout, os.Stderr, "/usr/bin/cwctl", "status")
        if err != nil {
            logrus.Error(err)
        }
        return nil
    })
}

func runControlCmd(cli *CWMan, args []string, action func(*container.Container) error) error {
    if len(args) == 1 && args[0] == "all" {
        containers, err := cli.ListAll()
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
            if err := cli.runContainerAction(id, action); err != nil {
                return err
            }
        }
    }
    return nil
}

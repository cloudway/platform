package cmds

import (
    "github.com/cloudway/platform/sandbox"
    "github.com/cloudway/platform/pkg/mflag"
)

func (cli *CWCtl) CmdStart(args ...string) error {
    cmd := cli.Subcmd("start")
    cmd.Require(mflag.Exact, 0)
    cmd.ParseFlags(args, true)
    return sandbox.NewApplication().Start()
}

func (cli *CWCtl) CmdStop(args ...string) error {
    cmd := cli.Subcmd("stop")
    cmd.Require(mflag.Exact, 0)
    cmd.ParseFlags(args, true)
    return sandbox.NewApplication().Stop()
}

func (cli *CWCtl) CmdRestart(args ...string) error {
    cmd := cli.Subcmd("restart")
    cmd.Require(mflag.Exact, 0)
    cmd.ParseFlags(args, true)
    return sandbox.NewApplication().Restart()
}

package cmds

import (
    "os"
    "github.com/cloudway/platform/sandbox"
    "github.com/cloudway/platform/pkg/mflag"
)

func (cli *CWCtl) CmdStart(args ...string) error {
    cmd := cli.Subcmd("start")
    cmd.Require(mflag.Exact, 0)
    cmd.ParseFlags(args, true)
    return sandbox.New().Start()
}

func (cli *CWCtl) CmdStop(args ...string) error {
    cmd := cli.Subcmd("stop")
    cmd.Require(mflag.Exact, 0)
    cmd.ParseFlags(args, true)
    return sandbox.New().Stop()
}

func (cli *CWCtl) CmdRestart(args ...string) error {
    cmd := cli.Subcmd("restart")
    cmd.Require(mflag.Exact, 0)
    cmd.ParseFlags(args, true)
    return sandbox.New().Restart()
}

func (cli *CWCtl) CmdStatus(args ...string) error {
    cmd := cli.Subcmd("status")
    cmd.Require(mflag.Exact, 0)
    cmd.ParseFlags(args, true)
    return sandbox.New().Control("status", false, false)
}

func (cli *CWCtl) CmdDump(args ...string) error {
    cmd := cli.Subcmd("dump")
    cmd.Require(mflag.Exact, 0)
    cmd.ParseFlags(args, true)
    return sandbox.New().Dump(os.Stdout)
}

func (cli *CWCtl) CmdRestore(args ...string) error {
    cmd := cli.Subcmd("restore")
    cmd.Require(mflag.Exact, 0)
    cmd.ParseFlags(args, true)
    return sandbox.New().Restore(os.Stdin)
}

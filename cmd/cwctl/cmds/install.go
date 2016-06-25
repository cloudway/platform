package cmds

import (
    "os"
    "github.com/cloudway/platform/sandbox"
    "github.com/cloudway/platform/pkg/mflag"
)

func (cli *CWCtl) CmdInstall(args ...string) error {
    if os.Getuid() != 0 {
        return os.ErrPermission
    }

    cmd := cli.Subcmd("install")
    cmd.Require(mflag.Min, 1)
    cmd.Require(mflag.Max, 2)
    cmd.ParseFlags(args, false)

    box := sandbox.New()
    if cmd.NArg() == 1 {
        return box.Install(cmd.Arg(0), "", os.Stdin)
    } else {
        return box.Install(cmd.Arg(0), cmd.Arg(1), nil)
    }
}

package cmds

import (
    "os"
    "github.com/cloudway/platform/sandbox"
    "github.com/cloudway/platform/pkg/mflag"
)

func (cli *CWCtl) CmdSetenv(args ...string) error {
    if os.Getuid() != 0 {
        return os.ErrPermission
    }

    cmd := cli.Cli.Subcmd("setenv", []string{"NAME VALUE"}, "Set application environment", true)
    export := cmd.Bool([]string{"-export"}, false, "Export the environment variable")
    cmd.Require(mflag.Exact, 2)
    cmd.ParseFlags(args, false)

    box := sandbox.New()
    key, val := cmd.Arg(0), cmd.Arg(1)
    if val == "" {
        box.Unsetenv(key)
        return nil
    } else {
        return box.Setenv(key, val, *export)
    }
}

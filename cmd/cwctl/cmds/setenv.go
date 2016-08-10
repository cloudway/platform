package cmds

import (
    "os"
    "strings"
    "github.com/cloudway/platform/sandbox"
    "github.com/cloudway/platform/pkg/mflag"
)

func (cli *CWCtl) CmdSetenv(args ...string) error {
    if os.Getuid() != 0 {
        return os.ErrPermission
    }

    cmd := cli.Cli.Subcmd("setenv", []string{"KEY VALUE", "KEY=VALUE...", "-d KEY...", },
                          "Set application environment variables", true)
    export := cmd.Bool([]string{"-export"}, false, "Export the environment variable")
    del := cmd.Bool([]string{"d"}, false, "Remove the environment variable")
    cmd.Require(mflag.Min, 1)
    cmd.ParseFlags(args, false)

    box := sandbox.New()

    // unset env var: setenv -d key1 key2 ...
    if *del {
        for i := 0; i < cmd.NArg(); i++ {
            box.Unsetenv(cmd.Arg(i))
        }
        return nil
    }

    // old format: setenv key value
    if cmd.NArg() == 2 && !strings.ContainsRune(cmd.Arg(0), '=') {
        key, val := cmd.Arg(0), cmd.Arg(1)
        return box.Setenv(key, val, *export)
    }

    // new format: setenv key1=value1 key2=value2 ...
    for i := 0; i < cmd.NArg(); i++ {
        if !strings.ContainsRune(cmd.Arg(i), '=') {
            cmd.Usage()
            os.Exit(1)
        }
    }
    for i := 0; i < cmd.NArg(); i++ {
        kv := cmd.Arg(i)
        i := strings.IndexRune(kv, '=')
        if err := box.Setenv(kv[:i], kv[i+1:], *export); err != nil {
            return err
        }
    }

    return nil
}

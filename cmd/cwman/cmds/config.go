package cmds

import (
    "fmt"
    "github.com/cloudway/platform/pkg/mflag"
    "github.com/cloudway/platform/container/conf"
)

func (cli *CWMan) CmdConfig(args ...string) error {
    var remove bool
    cmd := cli.Subcmd("config", "KEY [VALUE]")
    cmd.BoolVar(&remove, []string{"d"}, false, "Remove the key")
    cmd.Require(mflag.Min, 1)
    cmd.Require(mflag.Max, 2)
    cmd.ParseFlags(args, true)

    if err := conf.Initialize(); err != nil {
        return err
    }

    key := cmd.Arg(0)
    if remove {
        conf.Remove(key)
        return conf.Save()
    } else if cmd.NArg() == 2 {
        conf.Set(key, cmd.Arg(1))
        return conf.Save()
    } else {
        fmt.Println(conf.Get(key))
        return nil
    }
}

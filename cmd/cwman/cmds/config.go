package cmds

import (
	"fmt"
	"github.com/cloudway/platform/config"
	"github.com/cloudway/platform/pkg/mflag"
)

func (cli *CWMan) CmdConfig(args ...string) error {
	var remove bool
	cmd := cli.Subcmd("config", "KEY [VALUE]")
	cmd.BoolVar(&remove, []string{"d"}, false, "Remove the key")
	cmd.Require(mflag.Min, 1)
	cmd.Require(mflag.Max, 2)
	cmd.ParseFlags(args, true)

	if err := config.Initialize(); err != nil {
		return err
	}

	key := cmd.Arg(0)
	if remove {
		config.Remove(key)
		return config.Save()
	} else if cmd.NArg() == 2 {
		config.Set(key, cmd.Arg(1))
		return config.Save()
	} else {
		fmt.Println(config.Get(key))
		return nil
	}
}

package cmds

import (
	"context"
	"fmt"

	"github.com/cloudway/platform/config"
	"github.com/cloudway/platform/pkg/mflag"
)

func (cli *CWCli) CmdNamespace(args ...string) error {
	var set string
	var remove, force bool

	cmd := cli.Subcmd("namespace", "", "--set NAMESPACE", "--remove [-force]")
	cmd.Require(mflag.Exact, 0)
	cmd.StringVar(&set, []string{"-set"}, "", "Set to new namespace")
	cmd.BoolVar(&remove, []string{"-remove"}, false, "Remove the namespace")
	cmd.BoolVar(&force, []string{"-force"}, false, "Force to remove the namespace")
	cmd.ParseFlags(args, false)

	if err := cli.ConnectAndLogin(); err != nil {
		return err
	}

	var ctx = context.Background()

	if set == "" && !remove {
		namespace, err := cli.GetNamespace(ctx)
		if err == nil {
			fmt.Println(namespace)
		}
		return err
	}

	if remove {
		err := cli.RemoveNamespace(ctx, force)
		if err != nil {
			return err
		}
	}

	if set != "" {
		err := cli.SetNamespace(ctx, set)
		if err != nil {
			return err
		}
	}

	// logout after namespace changed
	if cli.host != "" {
		config.RemoveOption(cli.host, "token")
		config.Save()
	}

	return nil
}

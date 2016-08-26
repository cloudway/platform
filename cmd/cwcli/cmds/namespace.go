package cmds

import (
	"errors"
	"fmt"

	"github.com/cloudway/platform/pkg/mflag"
	"golang.org/x/net/context"
)

func (cli *CWCli) CmdNamespace(args ...string) error {
	var set string
	var remove, force bool

	cmd := cli.Subcmd("namespace", "")
	cmd.Require(mflag.Exact, 0)
	cmd.StringVar(&set, []string{"-set"}, "", "Set to new namespace")
	cmd.BoolVar(&remove, []string{"-remove"}, false, "Remove the namespace")
	cmd.BoolVar(&force, []string{"-force"}, false, "Force to remove the namespace")
	cmd.ParseFlags(args, true)

	if set != "" && remove {
		return errors.New("The --set and --remove flags are conflict")
	}

	if err := cli.ConnectAndLogin(); err != nil {
		return err
	}

	ctx := context.Background()

	if set != "" {
		return cli.SetNamespace(ctx, set)
	}

	if remove {
		return cli.RemoveNamespace(ctx, force)
	}

	namespace, err := cli.GetNamespace(ctx)
	if err == nil {
		fmt.Println(namespace)
	}
	return err
}

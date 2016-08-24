package cmds

import (
	"github.com/cloudway/platform/pkg/mflag"
	"golang.org/x/net/context"
)

func (cli *CWMan) CmdDeploy(args ...string) error {
	cmd := cli.Subcmd("deploy", "NAME NAMESPACE PATH")
	cmd.Require(mflag.Exact, 3)
	cmd.ParseFlags(args, true)

	ctx := context.Background()
	name, namespace, path := cmd.Arg(0), cmd.Arg(1), cmd.Arg(2)

	containers, err := cli.FindApplications(ctx, name, namespace)
	if err != nil {
		return err
	}
	for _, c := range containers {
		err = c.Deploy(ctx, path)
		if err != nil {
			return err
		}
	}
	return nil
}

package cmds

import (
	"github.com/cloudway/platform/container"
	"github.com/cloudway/platform/pkg/mflag"
	"golang.org/x/net/context"
)

func (cli *CWMan) CmdDeploy(args ...string) (err error) {
	cmd := cli.Subcmd("deploy", "NAME NAMESPACE PATH [ID]...")
	cmd.Require(mflag.Min, 3)
	cmd.ParseFlags(args, true)

	ctx := context.Background()
	name, namespace, path := cmd.Arg(0), cmd.Arg(1), cmd.Arg(2)

	var containers []*container.Container
	if ids := cmd.Args()[3:]; len(ids) == 0 {
		containers, err = cli.FindApplications(ctx, name, namespace)
		if err != nil {
			return err
		}
	} else {
		containers = make([]*container.Container, len(ids))
		for i, id := range ids {
			containers[i], err = cli.Inspect(ctx, id)
			if err != nil {
				return err
			}
		}
	}

	for _, c := range containers {
		er := c.Deploy(ctx, path)
		if er != nil {
			err = er
		}
	}
	return err
}

package cmds

import (
	"fmt"
	"github.com/cloudway/platform/container"
	"github.com/cloudway/platform/pkg/mflag"
	"golang.org/x/net/context"
)

func (cli *CWMan) CmdRun(args ...string) error {
	cmd := cli.Subcmd("run", "CONTAINER [COMMAND [ARG...]]")
	user := cmd.String([]string{"u", "-user"}, "", "Username")
	cmd.Require(mflag.Min, 1)
	cmd.ParseFlags(args, true)

	return cli.runContainerAction(context.Background(), cmd.Arg(0), func(ctx context.Context, c *container.Container) error {
		cargs := cmd.Args()[1:]
		if len(cargs) == 0 {
			cargs = []string{"/usr/bin/cwsh"}
		}

		err := c.Run(ctx, *user, cargs...)
		if _, ok := err.(container.StatusError); !ok {
			return err
		} else {
			return nil
		}
	})
}

func (cli *CWMan) runContainerAction(ctx context.Context, id string, action func(context.Context, *container.Container) error) error {
	service, name, namespace := container.SplitNames(id)
	if name != "" && namespace != "" {
		var containers []*container.Container
		var err error

		if service == "" {
			// assume the key is 'name-namespace'
			containers, err = cli.FindApplications(ctx, name, namespace)
		} else if service == "*" {
			// assume the key is '*.name-namespace'
			containers, err = cli.FindAll(ctx, name, namespace)
			if err == nil {
				err = container.ResolveServiceDependencies(containers)
			}
		} else {
			// assume the key is 'service.name-namespace'
			containers, err = cli.FindService(ctx, name, namespace, service)
		}

		if err != nil {
			return err
		}
		if len(containers) == 0 {
			return fmt.Errorf("%s: Not found", id)
		}

		for _, c := range containers {
			if err = action(ctx, c); err != nil {
				return err
			}
		}
		return nil
	} else {
		// assume the key is an application id
		c, err := cli.Inspect(ctx, id)
		if err != nil {
			return err
		}
		return action(ctx, c)
	}
}

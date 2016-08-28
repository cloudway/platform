package cmds

import (
	"fmt"

	"github.com/cloudway/platform/pkg/manifest"
	"github.com/cloudway/platform/pkg/mflag"
	"golang.org/x/net/context"
)

func (cli *CWCli) CmdPlugin(args ...string) error {
	var framework, service bool
	var category manifest.Category

	cmd := cli.Subcmd("plugin", "")
	cmd.Require(mflag.Min, 0)
	cmd.Require(mflag.Max, 1)
	cmd.BoolVar(&framework, []string{"F"}, false, "Show framework plugins")
	cmd.BoolVar(&service, []string{"s"}, false, "Show service plugins")
	cmd.ParseFlags(args, true)

	if err := cli.ConnectAndLogin(); err != nil {
		return err
	}

	if cmd.NArg() == 0 {
		if framework && !service {
			category = manifest.Framework
		} else if !framework && service {
			category = manifest.Service
		}

		plugins, err := cli.GetInstalledPlugins(context.Background(), category)
		if err != nil {
			return err
		}
		for _, p := range plugins {
			fmt.Fprintf(cli.stdout, "%-15s %s\n", p.Name, p.DisplayName)
		}
	} else {
		plugin, err := cli.GetPluginInfo(context.Background(), cmd.Arg(0))
		if err != nil {
			return err
		}

		fmt.Fprintf(cli.stdout, "Name:           %s\n", plugin.Name)
		fmt.Fprintf(cli.stdout, "Display Name:   %s\n", plugin.DisplayName)
		fmt.Fprintf(cli.stdout, "Description:    %s\n", plugin.Description)
		fmt.Fprintf(cli.stdout, "Version:        %s\n", plugin.Version)
		fmt.Fprintf(cli.stdout, "Vendor:         %s\n", plugin.Vendor)
		fmt.Fprintf(cli.stdout, "Category:       %s\n", plugin.Category)
		fmt.Fprintf(cli.stdout, "Base Image:     %s\n", plugin.BaseImage)
	}

	return nil
}

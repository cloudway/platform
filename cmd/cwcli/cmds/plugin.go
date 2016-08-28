package cmds

import (
	"archive/tar"
	"fmt"
	"io/ioutil"
	"os"

	"github.com/cloudway/platform/pkg/archive"
	"github.com/cloudway/platform/pkg/manifest"
	"github.com/cloudway/platform/pkg/mflag"
	"golang.org/x/net/context"
)

const pluginCmdUsage = `Usage: cwcli plugin
   or: cwcli plugin:install PATH
   or: cwcli plugin:remove TAG
`

func (cli *CWCli) CmdPlugin(args ...string) (err error) {
	var help bool
	var framework, service bool
	var category manifest.Category
	var userDefined bool

	cmd := cli.Subcmd("plugin", "")
	cmd.Require(mflag.Min, 0)
	cmd.Require(mflag.Max, 1)
	cmd.BoolVar(&help, []string{"h", "-help"}, false, "Print usage")
	cmd.BoolVar(&framework, []string{"F", "-framework"}, false, "Show framework plugins")
	cmd.BoolVar(&service, []string{"s", "-service"}, false, "Show service plugins")
	cmd.BoolVar(&userDefined, []string{"u", "-user"}, false, "Show user defined plugins")
	cmd.ParseFlags(args, false)

	if help {
		fmt.Fprintln(cli.stdout, pluginCmdUsage)
		os.Exit(0)
	}

	if err = cli.ConnectAndLogin(); err != nil {
		return err
	}

	if cmd.NArg() == 0 {
		if framework && !service {
			category = manifest.Framework
		} else if !framework && service {
			category = manifest.Service
		}

		var plugins []*manifest.Plugin
		if userDefined {
			plugins, err = cli.GetUserPlugins(context.Background(), category)
		} else {
			plugins, err = cli.GetInstalledPlugins(context.Background(), category)
		}
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

func (cli *CWCli) CmdPluginInstall(args ...string) (err error) {
	cmd := cli.Subcmd("plugin:install", "PATH")
	cmd.Require(mflag.Exact, 1)
	cmd.ParseFlags(args, true)
	path := cmd.Arg(0)

	if err = cli.ConnectAndLogin(); err != nil {
		return err
	}

	var file *os.File

	if st, err := os.Stat(path); err != nil {
		return err
	} else if st.IsDir() {
		file, err = makeArchive(path)
		if file != nil {
			defer func() {
				file.Close()
				os.Remove(file.Name())
			}()
		}
		if err != nil {
			return err
		}
	} else {
		file, err = os.Open(path)
		if err != nil {
			return err
		}
		defer file.Close()
	}

	return cli.InstallPlugin(context.Background(), file)
}

func makeArchive(path string) (file *os.File, err error) {
	file, err = ioutil.TempFile("", "plugin")
	if err != nil {
		return
	}

	tw := tar.NewWriter(file)
	if err = archive.CopyFileTree(tw, "", path, nil, false); err != nil {
		return
	}
	tw.Close()

	_, err = file.Seek(0, 0)
	return
}

func (cli *CWCli) CmdPluginRemove(args ...string) (err error) {
	cmd := cli.Subcmd("plugin:remove", "TAG")
	cmd.Require(mflag.Exact, 1)
	cmd.ParseFlags(args, false)

	if err = cli.ConnectAndLogin(); err != nil {
		return err
	}
	return cli.RemovePlugin(context.Background(), cmd.Arg(0))
}

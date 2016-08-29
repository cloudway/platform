package cmds

import (
	"fmt"
	"os"

	"github.com/cloudway/platform/pkg/mflag"
	"golang.org/x/net/context"
)

const namespaceCmdUsage = `Usage: cwcli namespace
   or: cwcli namespace:set NAMESPACE
   or: cwcli namespace:rm [-force]
`

func (cli *CWCli) CmdNamespace(args ...string) error {
	var help bool

	cmd := cli.Subcmd("namespace", "")
	cmd.Require(mflag.Exact, 0)
	cmd.BoolVar(&help, []string{"-help"}, false, "Print usage")
	cmd.ParseFlags(args, false)

	if help {
		fmt.Fprintln(cli.stdout, namespaceCmdUsage)
		os.Exit(0)
	}

	if err := cli.ConnectAndLogin(); err != nil {
		return err
	}

	namespace, err := cli.GetNamespace(context.Background())
	if err == nil {
		fmt.Println(namespace)
	}
	return err
}

func (cli *CWCli) cmdNamespaceSet(args ...string) error {
	cmd := cli.Subcmd("namespace:set", "NAMESPACE")
	cmd.Require(mflag.Exact, 1)
	cmd.ParseFlags(args, true)

	if err := cli.ConnectAndLogin(); err != nil {
		return err
	}
	return cli.SetNamespace(context.Background(), cmd.Arg(0))
}

func (cli *CWCli) CmdNamespaceRm(args ...string) error {
	var force bool

	cmd := cli.Subcmd("namespace:rm", "NAMESPACE")
	cmd.Require(mflag.Exact, 0)
	cmd.BoolVar(&force, []string{"f", "-force"}, false, "Force to remove the namespace")
	cmd.ParseFlags(args, true)

	if err := cli.ConnectAndLogin(); err != nil {
		return err
	}
	return cli.RemoveNamespace(context.Background(), force)
}

package cmds

import (
	"os"

	"github.com/cloudway/platform/pkg/mflag"
	"github.com/cloudway/platform/pkg/serverlog"
	"golang.org/x/net/context"
)

func (cli *CWMan) CmdDeploy(args ...string) (err error) {
	cmd := cli.Subcmd("deploy", "NAME NAMESPACE")
	cmd.Require(mflag.Exact, 2)
	cmd.ParseFlags(args, true)

	name, namespace := cmd.Arg(0), cmd.Arg(1)
	log := serverlog.Encap(os.Stdout, os.Stderr)
	return cli.DeployRepo(context.Background(), name, namespace, os.Stdin, log)
}

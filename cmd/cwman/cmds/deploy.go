package cmds

import (
	"os"

	"github.com/cloudway/platform/pkg/mflag"
	"github.com/cloudway/platform/scm"
	"golang.org/x/net/context"
)

func (cli *CWMan) CmdDeploy(args ...string) (err error) {
	cmd := cli.Subcmd("deploy", "NAME NAMESPACE [ID]...")
	cmd.Require(mflag.Min, 2)
	cmd.ParseFlags(args, true)

	name, namespace, ids := cmd.Arg(0), cmd.Arg(1), cmd.Args()[2:]
	return scm.DeployRepository(cli.DockerClient, context.Background(), name, namespace, ids, os.Stdin)
}

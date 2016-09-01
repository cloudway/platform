package cmds

import (
	"compress/gzip"
	"os"

	"github.com/cloudway/platform/pkg/archive"
	"github.com/cloudway/platform/pkg/mflag"
	"github.com/cloudway/platform/sandbox"
)

func (cli *CWCtl) CmdBuild(args ...string) (err error) {
	cmd := cli.Subcmd("build")
	cmd.Require(mflag.Exact, 0)
	cmd.ParseFlags(args, true)

	box := sandbox.New()

	// extract the repository archive
	zr, err := gzip.NewReader(os.Stdin)
	if err != nil {
		return err
	}

	err = archive.ExtractFiles(box.RepoDir(), zr)
	if err != nil {
		return err
	}

	// run build command
	return box.Build()
}

package cmds

import "github.com/cloudway/platform/sandbox"

func (cli *CWCtl) CmdRun(args ...string) error {
	return sandbox.New().Run()
}

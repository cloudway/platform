package cmds

import (
    "os"
    "github.com/cloudway/platform/sandbox"
)

func (cli *CWCtl) CmdUpload(args ...string) error {
    app := sandbox.NewApplication()
    return app.Upload(os.Stdin)
}

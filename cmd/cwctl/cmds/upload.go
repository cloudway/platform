package cmds

import (
    "io"
    "os"
    "github.com/cloudway/platform/sandbox"
    "github.com/cloudway/platform/pkg/files"
)

func (cli *CWCtl) CmdUpload(args ...string) error {
    app := sandbox.NewApplication()

    // Save upload file in the deployment directory
    err := saveUploadFile(app.DeployDir(), os.Stdin)
    if err != nil {
        return err
    }

    // Restart the application to make deployment take effect
    return app.Restart()
}

func saveUploadFile(dir string, r io.Reader) (err error) {
    f, err := files.TempFile(dir, "deploy", ".tar")
    if err != nil {
        return err
    }
    _, err = io.Copy(f, r)
    f.Close()
    return err
}

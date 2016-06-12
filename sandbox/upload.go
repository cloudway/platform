package sandbox

import (
    "io"
    "io/ioutil"
    "os"
    "github.com/cloudway/platform/pkg/archive"
    "github.com/cloudway/platform/pkg/files"
)

func (app *Application) Upload(r io.Reader) (err error) {
    // create temporary directory to recieve upload archive
    tmpdir, err := ioutil.TempDir("", "upload")
    if err != nil {
        return err
    }
    defer os.RemoveAll(tmpdir)

    // extract input archive file into temporary directory
    if err = archive.ExtractFiles(tmpdir, r); err != nil {
        return err
    }

    // stop application
    if err = app.Stop(); err != nil {
        return err
    }

    // move files into destination
    err = files.MoveFiles(tmpdir, app.RepoDir())
    if err != nil && !os.IsNotExist(err) {
        return err
    }

    // restart the application
    return app.Start()
}

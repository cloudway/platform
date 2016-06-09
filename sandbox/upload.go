package sandbox

import (
    "io"
    "io/ioutil"
    "os"
    "path/filepath"
)

func (app *Application) Upload(r io.Reader) (err error) {
    // create temporary directory to recieve upload archive
    tmpdir, err := ioutil.TempDir("", "upload")
    if err != nil {
        return err
    }
    defer os.RemoveAll(tmpdir)

    // extract input archive file into temporary directory
    if err = app.ExtractFiles(tmpdir, r); err != nil {
        return err
    }

    // stop application
    if err = app.Stop(); err != nil {
        return err
    }

    // move files into destination
    err = app.MoveFiles(app.RepoDir(), filepath.Join(tmpdir, "repo"))
    if err != nil && !os.IsNotExist(err) {
        return err
    }
    err = app.MoveFiles(app.DataDir(), filepath.Join(tmpdir, "data"))
    if err != nil && !os.IsNotExist(err) {
        return err
    }
    err = app.MoveFiles(app.LogDir(), filepath.Join(tmpdir, "logs"))
    if err != nil && !os.IsNotExist(err) {
        return err
    }

    // restart the application
    return app.Start()
}

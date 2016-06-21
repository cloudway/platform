package sandbox

import (
    "os"
    "strings"
    "path/filepath"
    "io/ioutil"
    "github.com/cloudway/platform/pkg/archive"
    "github.com/cloudway/platform/pkg/files"
)

func (app *Application) Deploy() error {
    base := app.DeployDir()

    deployments, err := getDeployments(base)
    if err != nil {
        return err
    }
    if len(deployments) == 0 {
        return nil
    }
    defer removeDeployments(base, deployments)

    latest := getLatestDeployment(deployments)
    return app.deploy(latest.Name())
}

func getDeployments(deployDir string) ([]os.FileInfo, error) {
    f, err := os.Open(deployDir)
    if err != nil {
        return nil, err
    }
    files, err := f.Readdir(0)
    if err != nil {
        return nil, err
    }

    var deployments []os.FileInfo
    for _, fi := range files {
        if strings.HasPrefix(fi.Name(), "deploy") && strings.HasSuffix(fi.Name(), ".tar") {
            deployments = append(deployments, fi)
        }
    }
    return deployments, nil
}

func removeDeployments(deployDir string, deployments []os.FileInfo) {
    for _, d := range deployments {
        os.Remove(filepath.Join(deployDir, d.Name()))
    }
}

func getLatestDeployment(deployments []os.FileInfo) os.FileInfo {
    var last *os.FileInfo
    for _, d := range deployments {
        if last == nil {
            last = &d
        } else if (d.ModTime().After((*last).ModTime())) {
            last = &d
        }
    }
    return *last
}

func (app *Application) deploy(name string) (err error) {
    f, err := os.Open(filepath.Join(app.DeployDir(), name))
    if err != nil {
        return err
    }
    defer f.Close()

    // Create temporary directory to extract deployment
    tmpdir, err := ioutil.TempDir("", "deploy")
    if err != nil {
        return err
    }
    defer os.RemoveAll(tmpdir)

    // extract archive file into temporary directory
    if err = archive.ExtractFiles(tmpdir, f); err != nil {
        return err
    }

    // move files into destination
    err = files.MoveFiles(tmpdir, app.RepoDir())
    if err != nil && !os.IsNotExist(err) {
        return err
    }

    return nil
}
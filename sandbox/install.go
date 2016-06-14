package sandbox

import (
    "fmt"
    "io"
    "os"
    "strings"
    "errors"
    "path/filepath"
    "github.com/Sirupsen/logrus"
    "github.com/cloudway/platform/pkg/manifest"
    "github.com/cloudway/platform/pkg/archive"
    "github.com/cloudway/platform/pkg/files"
)

// Install plugin in the application. The plugin directory should already
// been populated from broker.
func (app *Application) Install(name string, source string, input io.Reader) error {
    if os.Getuid() != 0 {
        return os.ErrPermission
    }

    var err error

    // create plugin directory
    target := filepath.Join(app.HomeDir(), name)
    if _, err = os.Stat(target); err == nil {
        return fmt.Errorf("Plugin already installed: %s", name)
    }
    if err = os.Mkdir(target, 0755); err != nil {
        return err
    }

    if input != nil {
        err = archive.ExtractFiles(target, input)
    } else {
        err = files.MoveFiles(source, target)
    }

    if err == nil {
        err = app.installPlugin(target)
    }

    if err != nil {
        // remove plugin directory if error occurred
        os.RemoveAll(target)
    }
    return err
}

func (app *Application) installPlugin(target string) error {
    // load plugin manifest from target directory
    meta, err := manifest.Load(target)
    if err != nil {
        return err
    }
    name := meta.Name

    // add environemnt variables
    app.Setenv("CLOUDWAY_" + strings.ToUpper(name) + "_DIR", target, false)
    app.Setenv("CLOUDWAY_" + strings.ToUpper(name) + "_VERSION", meta.Version, false)
    if meta.IsFramework() {
        app.Setenv("CLOUDWAY_FRAMEWORK", name, false)
        app.Setenv("CLOUDWAY_FRAMEWORK_DIR", target, false)
    }

    // create log dir
    logdir := filepath.Join(app.LogDir(), name)
    os.MkdirAll(logdir, 0750)
    os.Chown(logdir, app.uid, app.gid)
    app.Setenv("CLOUDWAY_" + strings.ToUpper(name) + "_LOG_DIR", logdir, false)

    // run install script for non-framework plugin
    if meta.IsLibrary() {
        if err = runPluginAction(target, nil, "install"); err != nil {
            logrus.WithError(err).Error("run install script failed")
            return err
        }
    }

    // run setup script to setup the plugin
    if err = runPluginAction(target, makeExecEnv(app.Environ()), "setup"); err != nil {
        logrus.WithError(err).Error("run setup script failed")
        return err
    }

    // remove unused setup scripts
    os.Remove(filepath.Join(target, "bin", "install"))
    os.Remove(filepath.Join(target, "bin", "setup"))

    // populate repository for framework plugin
    if meta.IsFramework() {
        if err = app.populateRepository(target, ""); err != nil {
            logrus.WithError(err).Error("Failed to populate repository")
            return err
        }
    }

    // change owner of plugin directory
    chownR(target, app.uid, app.gid)
    chownR(filepath.Join(target, "manifest"), 0, app.gid)

    return nil
}

func (app *Application) populateRepository(path, url string) error {
    if url == "" {
        return app.populateFromTemplate(path)
    } else {
        return app.populateFromURL(url)
    }
}

func (app *Application) populateFromTemplate(basedir string) error {
    t := filepath.Join(basedir, "template")
    if fi, err := os.Stat(t); err != nil || !fi.IsDir() {
        return nil
    } else {
        if err := files.MoveFiles(t, app.RepoDir()); err == nil {
            err = chownR(app.RepoDir(), app.uid, app.gid)
        }
        return err
    }
}

func (app *Application) populateFromURL(url string) error {
    return errors.New("NYI")
}

func chownR(root string, uid, gid int) error {
    return filepath.Walk(root, func(path string, info os.FileInfo, err error) error {
        if err == nil {
            err = os.Lchown(path, uid, gid)
        }
        return err
    })
}

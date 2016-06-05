package sandbox

import (
    "fmt"
    "io"
    "os"
    "archive/tar"
    "strings"
    "strconv"
    "regexp"
    "errors"
    "path/filepath"
    "text/template"
    "github.com/Sirupsen/logrus"
    "github.com/cloudway/platform/plugin"
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
        err = app.extractPluginFiles(target, input)
    } else {
        err = app.copyPluginFiles(target, source)
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
    meta, err := plugin.Load(target)
    if err != nil {
        return err
    }
    name := meta.Name

    // add environemnt variables
    app.Setenv("CLOUDWAY_" + strings.ToUpper(name) + "_DIR", target)
    app.Setenv("CLOUDWAY_" + strings.ToUpper(name) + "_VERSION", meta.Version)
    if meta.IsFramework() {
        app.Setenv("CLOUDWAY_FRAMEWORK", name)
        app.Setenv("CLOUDWAY_FRAMEWORK_DIR", target)
    }

    // assigns private host/port
    app.createPrivateEndpoints(meta)

    // process templates by substitution with environment variables
    if err = processTemplates(target, app.Environ()); err != nil {
        return err
    }

    // run install script for non-framework plugin
    if !meta.IsFramework() {
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

    // populate repository for framework plugin
    if meta.IsFramework() {
        if err = app.populateRepository(target, ""); err != nil {
            logrus.WithError(err).Error("Failed to populate repository")
            return err
        }
    }

    // change owner of plugin directory
    return chownR(target, app.uid, app.gid)
}

func (app *Application) extractPluginFiles(target string, source io.Reader) error {
    tr := tar.NewReader(source)

    for {
        hdr, err := tr.Next()
        if err != nil {
            if err == io.EOF {
                return nil
            }
            return err
        }

        dst := filepath.Join(target, hdr.Name)
        switch hdr.Typeflag {
        case tar.TypeDir:
            logrus.Debugf("Creating directory: %s", dst)
            if err = os.MkdirAll(dst, os.FileMode(hdr.Mode)); err != nil {
                return err
            }
            os.Chtimes(dst, hdr.AccessTime, hdr.ChangeTime)

        case tar.TypeReg:
            logrus.Debugf("Extracting %s", dst)
            os.MkdirAll(filepath.Dir(dst), 0755)
            w, err := os.Create(dst)
            if err != nil {
                return err
            }
            _, err = io.Copy(w, tr);
            w.Close()
            if err != nil {
                return err
            }
            os.Chmod(dst, os.FileMode(hdr.Mode));
            os.Chtimes(dst, hdr.AccessTime, hdr.ChangeTime)

        default:
            return fmt.Errorf("Unable to extract file %s", hdr.Name) // FIXME
        }
    }
}

func (app *Application) copyPluginFiles(dst, src string) error {
    defer os.RemoveAll(src)

    return filepath.Walk(src, func(path string, info os.FileInfo, err error) error {
        if err != nil {
            return err
        }

        target, err := filepath.Rel(src, path)
        if err != nil {
            logrus.WithError(err).Debug("Failed to get relative path")
            return nil
        }
        target = filepath.Join(dst, target)
        logrus.Debugf("Copying %s to %s", path, target)

        if info.IsDir() {
            os.MkdirAll(target, info.Mode())
            return nil
        }

        in, err := os.Open(path)
        if err != nil {
            return err
        }
        defer in.Close()

        out, err := os.Create(target)
        if err != nil {
            return err
        }
        defer out.Close()

        _, err = io.Copy(out, in)
        if err != nil {
            return err
        }

        os.Chmod(target, info.Mode())
        os.Chtimes(target, info.ModTime(), info.ModTime())
        return nil
    })
}

func (app *Application) createPrivateEndpoints(meta *plugin.Plugin) {
    if len(meta.Endpoints) == 0 {
        return
    }

    host, err := os.Hostname()
    if err != nil {
        logrus.WithError(err).Error("Cannot retrieve host name")
        return
    }

    env := app.Environ()
    for _, ep := range meta.GetEndpoints(env) {
        var host_name, port_name = ep.PrivateHostName, ep.PrivatePortName

        if env[host_name] == "" {
            app.Setenv(host_name, host)
            env[host_name] = host
        }

        port := strconv.Itoa(int(ep.PrivatePort))
        if env[port_name] == "" {
            app.Setenv(port_name, port)
            env[port_name] = port
        }
    }
}

var _TEMPLATE_RE = regexp.MustCompile(`^\.?(.*)\.cwt$`)

func processTemplates(root string, env map[string]string) error {
    return filepath.Walk(root, func(path string, info os.FileInfo, err error) error {
        if err != nil {
            return err
        }

        filename := filepath.Base(path)
        m := _TEMPLATE_RE.FindStringSubmatch(filename)
        if m == nil {
            return nil
        }

        t, err := template.ParseFiles(path)
        if err != nil {
            logrus.Error(err)
            return nil
        }

        outname := filepath.Join(filepath.Dir(path), m[1])
        out, err := os.Create(outname)
        if err != nil {
            logrus.Error(err)
            return nil
        }
        defer out.Close()

        if err = t.Execute(out, env); err != nil {
            logrus.Error(err)
            return nil
        }
        return nil
    })
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
        if err := app.copyPluginFiles(app.RepoDir(), t); err == nil {
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

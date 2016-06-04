package sandbox

import (
    "fmt"
    "io"
    "os"
    "os/exec"
    "strings"
    "regexp"
    "path/filepath"
    "text/template"
    "github.com/Sirupsen/logrus"
    "github.com/cloudway/platform/plugin"
)

// Install plugin in the application. The plugin directory should already
// been populated from broker.
func (app *Application) Install(source string) error {
    if os.Getuid() != 0 {
        return os.ErrPermission
    }

    // remove source directory when install finished
    defer os.RemoveAll(source)

    // load plugin manifest from source directory
    meta, err := plugin.Load(source, nil)
    if err != nil {
        return err
    }

    name   := meta.Name
    target := filepath.Join(app.HomeDir(), name)
    meta.Path = target

    if _, err := os.Stat(target); err == nil {
        return fmt.Errorf("Plugin already installed: %s", name)
    }

    // move files from source to target
    if err = os.Rename(source, target); err != nil {
        if err = copyDir(source, target); err != nil {
            return err
        }
    }

    // add environemnt variables
    app.Setenv("CLOUDWAY_" + strings.ToUpper(name) + "_DIR", target)
    app.Setenv("CLOUDWAY_" + strings.ToUpper(name) + "_VERSION", meta.Version)
    if meta.IsFramework() {
        app.Setenv("CLOUDWAY_FRAMEWORK", name)
        app.Setenv("CLOUDWAY_FRAMEWORK_DIR", target)
    }

    if err = processTemplates(target, app.Environ()); err != nil {
        return err
    }
    if err = runPluginAction(target, app.Environ(), "install"); err != nil {
        return err
    }

    // change owner of plugin directory
    return filepath.Walk(target, func(path string, info os.FileInfo, err error) error {
        if err == nil {
            err = os.Lchown(path, app.uid, app.gid)
        }
        return err
    })
}

func copyDir(src, dst string) error {
    return filepath.Walk(src, func(path string, info os.FileInfo, err error) error {
        if err != nil {
            return err
        }

        target := dst + path[len(src):]
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
        return nil
    })
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

func runPluginAction(path string, env map[string]string, action string, args ...string) error {
    executable := filepath.Join(path, "bin", action)
    cmd := exec.Command(executable, args...)
    cmd.Env    = makeExecEnv(env)
    cmd.Stdin  = os.Stdin
    cmd.Stdout = os.Stdout
    cmd.Stderr = os.Stderr
    return cmd.Run()
}

func makeExecEnv(env map[string]string) []string {
    eenv := make([]string, 0, len(env))
    for k, v := range env {
        eenv = append(eenv, k+"="+v)
    }
    return eenv
}

package sandbox

import (
    "os"
    "os/exec"
    "syscall"
    "path/filepath"
    "regexp"
    "text/template"
    "github.com/Sirupsen/logrus"
)

func (app *Application) Start() error {
    app.CreatePrivateEndpoints("")
    if err := app.Deploy(); err != nil {
        return err
    }
    return app.Control("start", true, true)
}

func (app *Application) Stop() error {
    return app.Control("stop", true, false)
}

func (app *Application) Restart() error {
    if err := app.Stop(); err != nil {
        return err
    }
    return app.Start()
}

func (app *Application) Control(action string, enable_action_hooks, process_templates bool) error {
    plugins, err := app.GetPlugins()
    if err != nil {
        return err
    }

    if process_templates {
        env := app.Environ()
        for _, p := range plugins {
            if err := processTemplates(p.Path, env); err != nil {
                return err
            }
        }
    }

    eenv := makeExecEnv(app.Environ())

    if enable_action_hooks {
        if err := app.runActionHook("pre_" + action, eenv); err != nil {
            logrus.WithError(err).Errorf("Error exec 'pre_%s'", action)
        }
    }

    for _, p := range plugins {
        if err := runPluginAction(p.Path, eenv, "control", action); err != nil {
            return err
        }
    }

    if enable_action_hooks {
        if err := app.runActionHook("post_" + action, eenv); err != nil {
            logrus.WithError(err).Errorf("Error exec 'post_%s'", action)
        }
    }

    return nil
}

var _TEMPLATE_RE = regexp.MustCompile(`^\.?(.*)\.cwt$`)

func processTemplates(root string, env map[string]string) error {
    return filepath.Walk(root, func(path string, info os.FileInfo, err error) error {
        if err != nil {
            return err
        }

        filename := info.Name()
        m := _TEMPLATE_RE.FindStringSubmatch(filename)
        if m == nil {
            return nil
        }

        t, err := template.ParseFiles(path)
        if err != nil {
            return err
        }

        outname := filepath.Join(filepath.Dir(path), m[1])
        out, err := os.Create(outname)
        if err != nil {
            return err
        }
        defer out.Close()

        return t.Execute(out, env);
    })
}

func runPluginAction(path string, env []string, action string, args ...string) error {
    filename := filepath.Join(path, "bin", action)
    if _, err := os.Stat(filename); err != nil {
        if os.IsNotExist(err) {
            return nil
        }
        return err
    }

    cmd := exec.Command(filename, args...)
    cmd.Stdin  = nil
    cmd.Stdout = os.Stdout
    cmd.Stderr = os.Stderr
    cmd.Env    = env
    cmd.Dir    = path
    return RunCommand(cmd)
}

func (app *Application) runActionHook(action string, env []string) error {
    hook := filepath.Join(app.RepoDir(), ".cloudway", "hooks", action)
    if _, err := os.Stat(hook); err != nil {
        if os.IsNotExist(err) {
            return nil
        }
        return err
    }

    cmd := exec.Command(hook)
    cmd.Stdin  = nil
    cmd.Stdout = os.Stdout
    cmd.Stderr = os.Stderr
    cmd.Env    = env
    cmd.Dir    = app.HomeDir()
    return RunCommand(cmd)
}

func RunCommand(cmd *exec.Cmd) error {
    err := cmd.Run()
    if syserr, ok := err.(*os.SyscallError); ok && syserr.Err == syscall.ECHILD {
        return nil // ignore if the child process was reaped
    } else {
        return err
    }
}

func makeExecEnv(env map[string]string) []string {
    if env != nil {
        eenv := make([]string, 0, len(env))
        for k, v := range env {
            eenv = append(eenv, k+"="+v)
        }
        return eenv
    } else {
        return nil
    }
}

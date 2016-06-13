package sandbox

import (
    "os"
    "os/exec"
    "syscall"
    "path/filepath"
)

func (app *Application) Start() error {
    app.CreatePrivateEndpoints("")
    return app.Control("start", true, true)
}

func (app *Application) Stop() error {
    return app.Control("stop", true, false)
}

func (app *Application) Restart() error {
    app.CreatePrivateEndpoints("")
    return app.Control("restart", true, true)
}

func (app *Application) Control(action string, enable_action_hooks, process_templates bool) error {
    env  := app.Environ()
    eenv := makeExecEnv(env)

    plugins, err := app.GetPlugins()
    if err != nil {
        return err
    }

    if enable_action_hooks {
        if err := app.runActionHook("pre_" + action, eenv); err != nil {
            return err
        }
    }

    for _, p := range plugins {
        path := p.Path
        if process_templates {
            if err := processTemplates(path, env); err != nil {
                return err
            }
        }
        if err := runPluginAction(path, eenv, "control", action); err != nil {
            return err
        }
    }

    if enable_action_hooks {
        if err := app.runActionHook("post_" + action, eenv); err != nil {
            return err
        }
    }

    return nil
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
        // the child process is reaped, so ignore
        return nil
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

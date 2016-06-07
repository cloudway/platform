package sandbox

import (
    "os"
    "os/exec"
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

    if process_templates {
        if err := processTemplates(app.RepoDir(), env); err != nil {
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
    executable := filepath.Join(path, "bin", action)
    cmd := exec.Command(executable, args...)
    cmd.Stdin  = os.Stdin
    cmd.Stdout = os.Stdout
    cmd.Stderr = os.Stderr
    cmd.Env    = env
    cmd.Dir    = path
    return cmd.Run()
}

func (app *Application) runActionHook(action string, env []string) error {
    hook := filepath.Join(app.RepoDir(), ".cloudway", "hooks", action)
    if _, err := os.Stat(hook); err == nil {
        cmd := exec.Command(hook)
        cmd.Stdin  = os.Stdin
        cmd.Stdout = os.Stdout
        cmd.Stderr = os.Stderr
        cmd.Env    = env
        cmd.Dir    = app.HomeDir()
        return cmd.Run()
    }
    return nil
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

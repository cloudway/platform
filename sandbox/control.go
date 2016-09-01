package sandbox

import (
	"os"
	"os/exec"
	"path/filepath"
	"regexp"
	"text/template"

	"github.com/Sirupsen/logrus"
	"github.com/cloudway/platform/pkg/manifest"
)

func (box *Sandbox) Start() error {
	box.CreatePrivateEndpoints("")

	if err := box.Deploy(); err != nil {
		return err
	}

	box.SetActiveState(manifest.StateStarting)
	err := box.Control("start", true, true)
	if err != nil {
		box.SetActiveState(manifest.StateFailed)
	} else {
		box.SetActiveState(manifest.StateRunning)
	}
	return err
}

func (box *Sandbox) Stop() error {
	box.SetActiveState(manifest.StateStopping)
	err := box.Control("stop", true, false)
	if err != nil {
		box.SetActiveState(manifest.StateFailed)
	} else {
		box.SetActiveState(manifest.StateStopped)
	}
	return err
}

func (box *Sandbox) Restart() (err error) {
	if box.hasDeployments() {
		err = box.Stop()
		if err == nil {
			err = box.Start()
		}
	} else {
		box.CreatePrivateEndpoints("")
		box.SetActiveState(manifest.StateRestarting)
		err = box.Control("restart", true, true)
		if err != nil {
			box.SetActiveState(manifest.StateFailed)
		} else {
			box.SetActiveState(manifest.StateRunning)
		}
	}
	return err
}

func (box *Sandbox) Control(action string, enable_action_hooks, process_templates bool) error {
	plugins, err := box.Plugins()
	if err != nil {
		return err
	}

	if process_templates {
		env := box.Environ()
		for _, p := range plugins {
			if err := processTemplates(p.Path, env); err != nil {
				return err
			}
		}
	}

	eenv := makeExecEnv(box.Environ())

	if enable_action_hooks {
		if err := box.runActionHook("pre_"+action, eenv); err != nil {
			logrus.WithError(err).Errorf("Error exec 'pre_%s'", action)
		}
	}

	for _, p := range plugins {
		if err := runPluginAction(p.Path, p.Path, eenv, "control", action); err != nil {
			return err
		}
	}

	if enable_action_hooks {
		if err := box.runActionHook("post_"+action, eenv); err != nil {
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

		return t.Execute(out, env)
	})
}

func runPluginAction(path, dir string, env []string, action string, args ...string) error {
	filename := filepath.Join(path, "bin", action)
	if _, err := os.Stat(filename); err != nil {
		if os.IsNotExist(err) {
			return nil
		}
		return err
	}

	cmd := exec.Command(filename, args...)
	cmd.Stdin = nil
	cmd.Stdout = os.Stdout
	cmd.Stderr = os.Stderr
	cmd.Env = env
	cmd.Dir = dir
	return RunCommand(cmd)
}

func (box *Sandbox) runActionHook(action string, env []string) error {
	hook := filepath.Join(box.RepoDir(), ".cloudway", "hooks", action)
	if _, err := os.Stat(hook); err != nil {
		if os.IsNotExist(err) {
			return nil
		}
		return err
	}

	cmd := exec.Command(hook)
	cmd.Stdin = nil
	cmd.Stdout = os.Stdout
	cmd.Stderr = os.Stderr
	cmd.Env = env
	cmd.Dir = box.HomeDir()
	return RunCommand(cmd)
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

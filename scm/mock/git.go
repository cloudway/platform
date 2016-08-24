package mock

import (
	"os"
	"os/exec"
	"strings"
)

type Git struct {
	dir string
}

func NewGitRepo(dir string) Git {
	return Git{dir}
}

func (git Git) Chdir(newdir string) {
	git.dir = newdir
}

func (git Git) Command(arg ...string) *exec.Cmd {
	cmd := exec.Command("git", arg...)
	cmd.Dir = git.dir
	cmd.Stdout = os.Stdout
	cmd.Stderr = os.Stderr
	return cmd
}

func (git Git) Run(arg ...string) error {
	return git.Command(arg...).Run()
}

func (git Git) Output(arg ...string) (string, error) {
	cmd := exec.Command("git", arg...)
	cmd.Dir = git.dir
	out, err := cmd.Output()
	return string(out), err
}

func (git Git) Init() error {
	return git.Run("init")
}

func (git Git) InitBare() error {
	return git.Run("init", "--bare")
}

func (git Git) Config(key, value string) error {
	return git.Run("config", key, value)
}

func (git Git) GetConfig(key string) string {
	out, _ := git.Output("config", key)
	return strings.TrimSpace(out)
}

func (git Git) Commit(message string) error {
	return git.Run("commit", "-a", "-m", message)
}

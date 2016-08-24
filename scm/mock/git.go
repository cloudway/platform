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

func (git Git) Init(bare bool) error {
	args := []string{"init"}
	if bare {
		args = append(args, "--bare")
	}
	return git.Command(args...).Run()
}

func (git Git) Config(key, value string) error {
	return git.Run("config", key, value)
}

func (git Git) GetConfig(key string) string {
	cmd := git.Command("config", key)
	cmd.Stdout = nil
	output, _ := cmd.Output()
	return strings.TrimSpace(string(output))
}

func (git Git) Commit(message string) error {
	return git.Run("commit", "-a", "-m", message)
}

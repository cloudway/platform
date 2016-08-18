package cmds

import (
	"os"
	"strings"
	"syscall"

	"github.com/cloudway/platform/pkg/opts"
	"github.com/cloudway/platform/sandbox"
)

func (cli *CWCtl) CmdSh(args ...string) error {
	env := make(map[string]string)
	cmd := cli.Subcmd("sh", "Run shell command")
	cmd.Var(opts.NewMapOptsRef(&env, opts.ValidateEnv), []string{"e"}, "Set environment variables")
	cmd.ParseFlags(args, true)

	if cmd.NArg() == 0 {
		return os.ErrInvalid
	}

	runShell(cmd.Args(), env)
	return nil
}

func runShell(args []string, env map[string]string) {
	prog := args[0]
	cmd := []string{"/bin/bash"}
	cmd_i := []string{"/bin/bash", "--init-file", "/etc/cloudway/bashrc", "-i"}

	// Determine the command source
	//
	// There are two possible command sources:
	//
	// 1) authenticate with SSH public key, and key has command= section
	//    Get command and args from SSH_ORIGINAL_COMMAND
	// 2) Authenticate with other means
	//    Get command and args from os.Args
	orig_cmd := os.Getenv("SSH_ORIGINAL_COMMAND")
	if orig_cmd != "" {
		// extract the command which was replaced by SSH auth key command
		cmd = append(cmd, "-c", orig_cmd)
	} else {
		// use the command passed by the user through SSH
		// be careful not to recurse (ssh adds $SHELL -c)
		if len(args) >= 2 && args[0] == prog && args[1] == "-c" {
			// the final command in a single string
			if len(args) >= 3 && args[2] == prog {
				// avoid recursion, drop to shell
				cmd = cmd_i
			} else {
				cmd = append(cmd, "-c", strings.Join(args[2:], " "))
			}
		} else {
			if len(args) == 1 {
				cmd = cmd_i
			} else {
				cmd = append(cmd, "-c", strings.Join(args[1:], " "))
			}
		}
	}

	syscall.Exec(cmd[0], cmd, makeExecEnv(env))
	os.Exit(1)
}

func makeExecEnv(env map[string]string) []string {
	myenv := sandbox.New().Environ()
	for k, v := range env {
		myenv[k] = v
	}

	eenv := make([]string, 0, len(myenv))
	for k, v := range myenv {
		eenv = append(eenv, k+"="+v)
	}
	return eenv
}

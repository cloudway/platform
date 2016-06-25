package cmds

import (
    "os"
    "strings"
    "syscall"
    "github.com/cloudway/platform/sandbox"
)

func (cli *CWCtl) CmdSh(args ...string) error {
    if len(args) == 0 {
        return os.ErrInvalid
    }

    runShell(args)
    return nil
}

func runShell(args[]string) {
    env   := sandbox.New().Environ()
    prog  := args[0]
    cmd   := []string{"/bin/bash"}
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
                cmd = append(cmd, "-c")
                cmd = append(cmd, args[2:]...)
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

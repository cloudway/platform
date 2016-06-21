package cmds

import (
    "fmt"
    "os"
    "github.com/cloudway/platform/pkg/mflag"
    "github.com/cloudway/platform/container"
)

func (cli *CWMan) CmdDeploy(args ...string) error {
    cmd := cli.Subcmd("deploy", "CONTAINER FILE")
    cmd.Require(mflag.Exact, 2)
    cmd.ParseFlags(args, true)

    file := cmd.Arg(1)
    service, name, namespace := container.SplitNames(cmd.Arg(0))
    if name == "" || namespace == "" || service == "*" {
        return fmt.Errorf("Invalid container: %s", cmd.Arg(0))
    }

    containers, err := cli.FindApplications(name, namespace)
    if err != nil {
        return err
    }
    if len(containers) == 0 {
        return nil
    }

    fi, err := os.Stat(file)
    if err != nil {
        return err
    }
    r, err := os.Open(file)
    if err != nil {
        return err
    }
    defer r.Close()

    for _, c := range containers {
        r.Seek(0, os.SEEK_SET)
        err = c.Deploy(r, fi.Name(), fi.Size())
        if err != nil {
            return err
        }
    }
    return nil
}

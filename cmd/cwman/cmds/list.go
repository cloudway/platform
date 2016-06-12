package cmds

import (
    "fmt"
    "github.com/cloudway/platform/pkg/mflag"
    "github.com/cloudway/platform/container"
)

func (cli *CWMan) CmdList(args ...string) error {
    cmd := cli.Subcmd("list")
    cmd.Require(mflag.Exact, 0)
    cmd.ParseFlags(args, true)

    containers, err := container.All()
    if err != nil {
        return err
    }

    fmt.Printf("%-12s    %-10s    %-8s   %s\n", "CONTAINER ID", "CATEGORY", "STATUS", "DOMAIN NAME")
    for _, c := range containers {
        fmt.Printf("%-12s    %-10s    %-8s   %s\n", c.ID[:12], c.Category(), c.State, c.FQDN())
    }

    return nil
}

package cmds

import (
    "fmt"
    "github.com/cloudway/platform/pkg/mflag"
    "github.com/cloudway/platform/api"
)

func (cli *CWCli) CmdVersion(args ...string) error {
    cmd := cli.Subcmd("version", "")
    cmd.Require(mflag.Exact, 0)
    cmd.ParseFlags(args, true)

    fmt.Println("Client:")
    fmt.Printf(" Version:    %s\n", api.Version)
    fmt.Printf(" Git commit: %s\n", api.GitCommit)
    fmt.Printf(" Build time: %s\n", api.BuildTime)

    return nil
}

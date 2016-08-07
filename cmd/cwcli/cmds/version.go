package cmds

import (
    "fmt"
    "github.com/cloudway/platform/pkg/mflag"
    "github.com/cloudway/platform/api"
    "golang.org/x/net/context"
)

func (cli *CWCli) CmdVersion(args ...string) error {
    cmd := cli.Subcmd("version", "")
    cmd.Require(mflag.Exact, 0)
    cmd.ParseFlags(args, true)

    if err := cli.Connect(); err != nil {
        return err
    }

    v, err := cli.ServerVersion(context.Background())
    if err != nil {
        return err
    }

    fmt.Println("Client:")
    fmt.Printf(" Version:        %s\n", api.Version)
    fmt.Printf(" Git commit:     %s\n", api.GitCommit)
    fmt.Printf(" Build time:     %s\n", api.BuildTime)

    fmt.Printf("\nServer: %s\n", cli.host)
    fmt.Printf(" Version:        %s\n", v.Version)
    fmt.Printf(" Git commit:     %s\n", v.GitCommit)
    fmt.Printf(" Build Time:     %s\n", v.BuildTime)
    fmt.Printf(" Docker version: %s\n", v.DockerVersion)
    fmt.Printf(" OS/Arch:        %s/%s\n", v.Os, v.Arch)

    return nil
}

package cmds

import (
    "fmt"
    "strings"
    "golang.org/x/net/context"
    "github.com/cloudway/platform/pkg/mflag"
    "github.com/cloudway/platform/api/client"
)

func (cli *CWMan) CmdVersion(args ...string) error {
    cmd := cli.Subcmd("version SERVER-URL")
    cmd.Require(mflag.Exact, 1)
    cmd.ParseFlags(args, true)

    url := cmd.Arg(0)
    if !strings.HasSuffix(url, "/api") {
        url += "/api"
    }

    api, err := client.NewAPIClient(url, "", nil, nil)
    if err != nil {
        return err
    }

    version, err := api.ServerVersion(context.Background())
    if err != nil {
        return err
    }

    fmt.Printf("API version:    %s\n", version.Version)
    fmt.Printf("Docker version: %s\n", version.DockerVersion)
    fmt.Printf("OS/Arch:        %s/%s\n", version.Os, version.Arch)
    return nil
}

package cmds

import (
    "os"
    "github.com/spf13/cobra"
    "github.com/docker/engine-api/client"
    "github.com/cloudway/platform/proxy"
)

func init() {
    cmdUpdateProxy := &cobra.Command{
        Use:        "update-proxy",
        Short:      "Update reverse proxy",
        Run:        runUpdateProxyCmd,
        Hidden:     true,
    }
    RootCommand.AddCommand(cmdUpdateProxy)
}

func runUpdateProxyCmd(cmd *cobra.Command, args []string) {
    cli, err := client.NewEnvClient()
    check(err)

    prx, err := proxy.New(os.Getenv("CLOUDWAY_PROXY_HOST"))
    check(err)

    check(proxy.RunUpdater(cli, prx))
}

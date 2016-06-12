package cmds

import (
    "os"
    "github.com/spf13/cobra"
    "github.com/cloudway/platform/sandbox"
    "encoding/json"
    "github.com/cloudway/platform/pkg/manifest"
)

func init() {
    if os.Getuid() == 0 {
        cmdInfo := &cobra.Command{
            Use:    "info",
            Short:  "Output application environments",
            Run:    runInfoCmd,
        }

        cmdInfo.Flags().String("ip", "", "Set IP address for endpoints")
        RootCommand.AddCommand(cmdInfo)
    }
}

func runInfoCmd(cmd *cobra.Command, args []string) {
    ip, _ := cmd.Flags().GetString("ip")
    app := sandbox.NewApplication()

    env := app.ExportedEnviron()

    endpoints, err := app.GetEndpoints(ip)
    check(err)

    ps, err := app.GetPlugins()
    check(err)
    plugins := make([]*manifest.Plugin, 0, len(ps))
    for _, p := range ps {
        plugins = append(plugins, p)
    }

    info := manifest.ApplicationInfo{
        Env:        env,
        Endpoints:  endpoints,
        Plugins:    plugins,
    }

    check(json.NewEncoder(os.Stdout).Encode(&info))
}

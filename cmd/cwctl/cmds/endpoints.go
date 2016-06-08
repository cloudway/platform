package cmds

import (
    "os"
    "encoding/json"
    "github.com/spf13/cobra"
    "github.com/cloudway/platform/sandbox"
)

func init() {
    if os.Getuid() == 0 {
        cmdEndpoints := &cobra.Command{
            Use:    "endpoints",
            Short:  "Show endpoint information about the application",
            Run:    runEndpointsCmd,
            Hidden: true,
        }

        cmdEndpoints.Flags().String("ip", "", "Set IP address for endpoints")
        RootCommand.AddCommand(cmdEndpoints)
    }
}

func runEndpointsCmd(cmd *cobra.Command, args []string) {
    ip, _ := cmd.Flags().GetString("ip")
    app := sandbox.NewApplication()

    endpoints, err := app.GetEndpoints(ip)
    check(err)

    enc := json.NewEncoder(os.Stdout)
    enc.Encode(endpoints)
}

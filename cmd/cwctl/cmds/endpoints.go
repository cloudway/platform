package cmds

import (
    "os"
    "encoding/json"
    "github.com/spf13/cobra"
    "github.com/cloudway/platform/sandbox"
    "github.com/cloudway/platform/plugin"
)

func init() {
    cmdEndpoints := &cobra.Command{
        Use:    "endpoints",
        Short:  "Show endpoint information about the application",
        Run:    runEndpointsCmd,
        Hidden: true,
    }

    cmdEndpoints.Flags().String("ip", "", "Set IP address for endpoints")
    RootCommand.AddCommand(cmdEndpoints)
}

func runEndpointsCmd(cmd *cobra.Command, args []string) {
    var err error
    app := sandbox.NewApplication()

    ip, _ := cmd.Flags().GetString("ip")
    if ip == "" {
        ip, err = os.Hostname()
        check(err)
    }

    plugins, err := app.GetPlugins()
    check(err)

    endpoints := make([]*plugin.Endpoint, 0)
    for _, p := range plugins {
        endpoints = append(endpoints, p.GetEndpoints(ip)...)
    }

    enc := json.NewEncoder(os.Stdout)
    enc.Encode(endpoints)
}
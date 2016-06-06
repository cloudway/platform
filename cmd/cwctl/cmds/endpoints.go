package cmds

import (
    "os"
    "bytes"
    "io/ioutil"
    "path/filepath"
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

    var buf bytes.Buffer
    enc := json.NewEncoder(&buf)
    enc.Encode(endpoints)

    // Write the endpoint info to a file so it can be read
    // even if the container is stopped
    filename := filepath.Join(app.EnvDir(), ".endpoint")
    check(ioutil.WriteFile(filename, buf.Bytes(), 0600))
    buf.WriteTo(os.Stdout)
}

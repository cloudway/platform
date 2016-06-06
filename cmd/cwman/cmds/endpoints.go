package cmds

import (
    "os"
    "bytes"
    "encoding/json"
    "github.com/spf13/cobra"
    "github.com/cloudway/platform/container"
)

func init() {
    cmdEndpoints := &cobra.Command{
        Use:     "endpoints",
        Short:   "Show endpoint information about the application",
        PreRunE: checkContainerArg,
        Run:     runEndpointsCmd,
    }
    RootCommand.AddCommand(cmdEndpoints)
}

func runEndpointsCmd(cmd *cobra.Command, args []string) {
    runContainerAction(args[0], func (c *container.Container) error {
        var b bytes.Buffer
        err := c.Exec("", nil, &b, os.Stderr, "/usr/bin/cwctl", "endpoints", "--ip", c.IP())
        if err != nil {
            return err
        }

        var out bytes.Buffer
        json.Indent(&out, b.Bytes(), "", "    ")
        out.WriteTo(os.Stdout)

        return nil
    })
}

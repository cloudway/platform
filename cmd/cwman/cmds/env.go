package cmds

import (
    "github.com/spf13/cobra"
    "github.com/cloudway/platform/container"
    "fmt"
)

func init() {
    cmdEnv := &cobra.Command{
        Use: "env CONTAINER",
        Short: "Show container environment variables",
        PreRunE: checkContainerArg,
        Run: runEnvCmd,
    }
    RootCommand.AddCommand(cmdEnv)
}

func runEnvCmd(cmd *cobra.Command, args []string) {
    runContainerAction(args[0], func (c *container.Container) error {
        info, err := c.GetInfo()
        if err != nil {
            return err
        }

        fmt.Printf("%s:\n", c.FQDN())
        for k, v := range info.Env {
            fmt.Printf("%s=%q\n", k, v)
        }
        fmt.Println()

        return nil
    })
}
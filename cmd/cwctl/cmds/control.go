package cmds

import (
    "github.com/spf13/cobra"
    "github.com/cloudway/platform/sandbox"
)

func init() {
    RootCommand.AddCommand(
        &cobra.Command{
            Use: "start",
            Short: "Start the application",
            Run: runControlCmd((*sandbox.Application).Start),
        },
    )

    RootCommand.AddCommand(
        &cobra.Command{
            Use: "stop",
            Short: "Stop the application",
            Run: runControlCmd((*sandbox.Application).Stop),
        },
    )

    RootCommand.AddCommand(
        &cobra.Command{
            Use: "restart",
            Short: "Restart the application",
            Run: runControlCmd((*sandbox.Application).Restart),
        },
    )
}

func runControlCmd(action func (*sandbox.Application) error) func (*cobra.Command, []string) {
    return func (cmd *cobra.Command, args []string) {
        app := sandbox.NewApplication()
        check(action(app))
    }
}

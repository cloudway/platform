package cmds

import (
    "github.com/spf13/cobra"
    "github.com/cloudway/platform/container"
)

func init() {
    RootCommand.AddCommand(
        &cobra.Command {
            Use: "start containers...",
            Short: "Start application containers",
            PreRunE: checkContainerArg,
            Run: runControlCmd((*container.Container).Start),
        },

        &cobra.Command {
            Use: "stop containers...",
            Short: "Stop application containers",
            PreRunE: checkContainerArg,
            Run: runControlCmd((*container.Container).Stop),
        },

        &cobra.Command {
            Use: "restart containers...",
            Short: "Restart application containers",
            PreRunE: checkContainerArg,
            Run: runControlCmd((*container.Container).Restart),
        },

        &cobra.Command {
            Use: "destroy containers...",
            Short: "Destroy application containers",
            PreRunE: checkContainerArg,
            Run: runControlCmd((*container.Container).Destroy),
        },
    )
}

func runControlCmd(action func (*container.Container) error) func (*cobra.Command, []string) {
    return func (cmd *cobra.Command, args []string) {
        if len(args) == 1 && args[0] == "all" {
            containers, err := container.All()
            check(err)
            for _, c := range containers {
                check(action(c))
            }
        } else {
            for _, id := range args {
                runContainerAction(id, action)
            }
        }
    }
}

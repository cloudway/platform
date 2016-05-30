package cmds

import (
    "errors"
    "github.com/spf13/cobra"
    "icloudway.com/platform/container"
)

func init() {
    RootCommand.AddCommand(
        &cobra.Command {
            Use: "start containers...",
            Short: "Start application containers",
            PreRunE: checkControlArgs,
            Run: runControlCmd((*container.Container).Start),
        },

        &cobra.Command {
            Use: "stop containers...",
            Short: "Stop application containers",
            PreRunE: checkControlArgs,
            Run: runControlCmd((*container.Container).Stop),
        },

        &cobra.Command {
            Use: "restart containers...",
            Short: "Restart application containers",
            PreRunE: checkControlArgs,
            Run: runControlCmd((*container.Container).Restart),
        },

        &cobra.Command {
            Use: "destroy containers...",
            Short: "Destroy application containers",
            PreRunE: checkControlArgs,
            Run: runControlCmd((*container.Container).Destroy),
        },
    )
}

func checkControlArgs(cmd *cobra.Command, args []string) error {
    if len(args) == 0 {
        return errors.New("you must provide the container id or name")
    }
    return nil
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

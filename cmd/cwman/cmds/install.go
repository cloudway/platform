package cmds

import (
    "github.com/spf13/cobra"
    "errors"
    "github.com/cloudway/platform/container"
)

func init() {
    cmdInstall := &cobra.Command{
        Use:     "install CONTAINER PATH",
        Short:   "Install a plugin to application container",
        PreRunE: checkInstallArgs,
        Run:     runInstallCmd,
    }
    RootCommand.AddCommand(cmdInstall)
}

func checkInstallArgs(cmd *cobra.Command, args []string) error {
    if len(args) != 2 {
        return errors.New("Invalid number of arguments")
    }
    return nil
}

func runInstallCmd(cmd *cobra.Command, args []string) {
    runContainerAction(args[0], func (c *container.Container) error {
        return c.Install(args[1])
    })
}

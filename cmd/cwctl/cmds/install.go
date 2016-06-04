package cmds

import (
    "os"
    "github.com/spf13/cobra"
    "github.com/cloudway/platform/sandbox"
)

func init() {
    if os.Getuid() == 0 {
        cmdInstall := &cobra.Command{
            Use:    "install PATH",
            Short:  "Install a plugin",
            Run:    runInstall,
        }
        RootCommand.AddCommand(cmdInstall)
    }
}

func runInstall(cmd *cobra.Command, args []string) {
    app := sandbox.NewApplication()
    check(app.Install(args[0]))
}

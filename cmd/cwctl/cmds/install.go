package cmds

import (
    "os"
    "errors"
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
    if len(args) == 1 {
        check(app.Install(args[0], "", os.Stdin))
    } else if len(args) == 2 {
        check(app.Install(args[0], args[1], nil))
    } else {
        check(errors.New("Invalid number of arguments"))
    }
}

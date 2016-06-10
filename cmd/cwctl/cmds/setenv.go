package cmds

import (
    "os"
    "errors"
    "github.com/spf13/cobra"
    "github.com/cloudway/platform/sandbox"
)

func init() {
    if os.Getuid() == 0 {
        cmdSetenv := &cobra.Command{
            Use:     "setenv NAME VALUE",
            Short:   "Set application environment",
            Run:     runSetenv,
            PreRunE: func(cmd *cobra.Command, args []string) error {
                if len(args) != 2 {
                    return errors.New("Invalid argument number")
                }
                return nil
            },
        }

        cmdSetenv.Flags().Bool("export", false, "Export the environment variable")
        RootCommand.AddCommand(cmdSetenv)
    }
}

func runSetenv(cmd *cobra.Command, args []string) {
    app := sandbox.NewApplication()
    name, value := args[0], args[1]
    export, _ := cmd.Flags().GetBool("export")
    check(app.Setenv(name, value, export))
}

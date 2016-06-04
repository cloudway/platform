package cmds

import (
    "github.com/spf13/cobra"
    "github.com/Sirupsen/logrus"
    "os"
)

var RootCommand = &cobra.Command{
    Use:    "cwctl",
    Short:  "Cloudway application management tool",
}

func init() {
    RootCommand.PersistentFlags().Bool("debug", false, "debugging mode")
    RootCommand.PersistentPreRun = func (cmd *cobra.Command, args []string) {
        if b, _ := RootCommand.Flags().GetBool("debug"); b {
            logrus.SetLevel(logrus.DebugLevel)
        }
    }
}

func check(err error) {
    if err != nil {
        logrus.Fatal(err)
        os.Exit(1)
    }
}

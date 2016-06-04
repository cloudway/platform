package cmds

import (
    "os"
    "os/signal"
    "syscall"
    "github.com/Sirupsen/logrus"
    "github.com/spf13/cobra"
)

func init() {
    cmdStart := &cobra.Command{
        Use:    "start",
        Short:  "Start the application",
        Run:    runStart,
    }
    RootCommand.AddCommand(cmdStart)
}

func runStart(cmd *cobra.Command, args []string) {
    logrus.Info("Application running")

    sigchan := make(chan os.Signal, 1)
    signal.Notify(sigchan, syscall.SIGTERM)

    select {
    case <-sigchan:
        logrus.Info("Application stopped")
        os.Exit(0)
    }
}

package cmds

import (
    "os"
    "os/signal"
    "syscall"
    "github.com/Sirupsen/logrus"
    "github.com/spf13/cobra"
    "github.com/cloudway/platform/sandbox"
)

func init() {
    cmdRun := &cobra.Command{
        Use:    "run",
        Short:  "Run the application",
        Run:    runRun,
        Hidden: true,
    }
    RootCommand.AddCommand(cmdRun)
}

func runRun(cmd *cobra.Command, args []string) {
    // reaping zombie processes
    signal.Ignore(syscall.SIGCHLD)

    // handle termination signals
    sigchan := make(chan os.Signal, 1)
    signal.Notify(sigchan, syscall.SIGHUP, syscall.SIGTERM, syscall.SIGINT)

    app := sandbox.NewApplication()

    for {
        sig := <-sigchan
        logrus.Infof("Received signal: %s\n", sig)
        switch sig {
        case syscall.SIGHUP:
            err := app.Restart()
            if err == nil {
                logrus.Info("Application restarted")
            } else {
                logrus.WithError(err).Error("Application restart failed")
            }

        default:
            err := app.Stop()
            if err == nil {
                logrus.Info("Application stopped")
                os.Exit(0)
            } else {
                logrus.WithError(err).Error("Application stop failed")
                os.Exit(1)
            }
        }
    }
}

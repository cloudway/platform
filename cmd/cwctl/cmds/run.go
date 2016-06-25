package cmds

import (
    "os"
    "os/signal"
    "syscall"
    "github.com/Sirupsen/logrus"
    "github.com/cloudway/platform/sandbox"
)

func (cli *CWCtl) CmdRun(args ...string) error {
    // reaping zombie processes
    signal.Ignore(syscall.SIGCHLD)

    // handle termination signals
    sigchan := make(chan os.Signal, 1)
    signal.Notify(sigchan, syscall.SIGHUP, syscall.SIGTERM, syscall.SIGINT)

    box := sandbox.New()

    for {
        sig := <-sigchan
        logrus.Infof("Received signal: %s\n", sig)
        switch sig {
        case syscall.SIGHUP:
            err := box.Restart()
            if err == nil {
                logrus.Info("Application restarted")
            } else {
                logrus.WithError(err).Error("Application restart failed")
            }

        default:
            err := box.Stop()
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

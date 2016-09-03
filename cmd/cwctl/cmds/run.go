package cmds

import (
	"os"
	"os/signal"
	"syscall"

	"github.com/Sirupsen/logrus"
	"github.com/cloudway/platform/sandbox"
)

func (cli *CWCtl) CmdRun(args ...string) error {
	// reap child processes
	go sandbox.Reap()

	// handle termination signals
	sigchan := make(chan os.Signal, 1)
	signal.Notify(sigchan, syscall.SIGHUP, syscall.SIGTERM, syscall.SIGINT, syscall.SIGUSR1)

	box := sandbox.New()
	sandbox.UpdateActiveState(box.ActiveState())

	for {
		sig := <-sigchan
		switch sig {
		case syscall.SIGHUP:
			logrus.Infof("Received signal: %s", sig)
			err := box.Restart()
			if err == nil {
				logrus.Info("Application restarted")
			} else {
				logrus.WithError(err).Error("Application restart failed")
			}

		case syscall.SIGUSR1:
			sandbox.UpdateActiveState(box.ActiveState())

		default:
			logrus.Infof("Received signal: %s", sig)
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

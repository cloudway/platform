package sandbox

import (
	"os"
	"os/signal"
	"syscall"

	"github.com/Sirupsen/logrus"
	"github.com/cloudway/platform/pkg/reap"
)

var reaper *reap.Reaper

func (box *Sandbox) Run() error {
	// reap child processes
	reapchan := make(chan reap.Child, 1)
	reaper = reap.New()
	defer reaper.Close()
	go reaper.Run(reapchan)

	go func() {
		for c := range reapchan {
			if c.Status.Exited() {
				logrus.Infof("child process %d terminated with exit status %d", c.Pid, c.Status.ExitStatus())
			} else if c.Status.Signaled() {
				logrus.Infof("child process %d terminated by signal %v", c.Pid, c.Status.Signal())
			} else {
				logrus.Infof("child process %d terminated with status %d", c.Pid, c.Status)
			}
		}
	}()

	// handle termination signals
	sigchan := make(chan os.Signal, 1)
	signal.Notify(sigchan, syscall.SIGHUP, syscall.SIGTERM, syscall.SIGINT, syscall.SIGUSR1)

	UpdateActiveState(box.ActiveState())

	for sig := range sigchan {
		switch sig {
		case syscall.SIGHUP:
			logrus.Infof("received signal: %s", sig)
			err := box.Restart()
			if err == nil {
				logrus.Info("application restarted")
			} else {
				logrus.WithError(err).Error("application restart failed")
			}

		case syscall.SIGUSR1:
			UpdateActiveState(box.ActiveState())

		default:
			logrus.Infof("received signal: %s", sig)
			return box.Stop()
		}
	}

	return nil
}

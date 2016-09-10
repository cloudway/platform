package cmds

import (
	"fmt"
	"os"
	"os/exec"
	"syscall"
	"time"

	"github.com/Sirupsen/logrus"
	"github.com/sevlyar/go-daemon"

	"github.com/cloudway/platform/pkg/mflag"
	"github.com/cloudway/platform/sandbox"
)

var (
	context *daemon.Context
	stop    = make(chan os.Signal, 1)
	done    = make(chan struct{})
)

func (cli *CWCtl) CmdDaemon(args ...string) error {
	var pidFile, logFile, workDir string

	cmd := cli.Subcmd("daemon")
	cmd.StringVar(&pidFile, []string{"-pid-file"}, "", "PID file name")
	cmd.StringVar(&logFile, []string{"-log-file"}, "", "Log file name")
	cmd.StringVar(&workDir, []string{"-work-dir"}, "", "Work directory")
	cmd.Require(mflag.Min, 1)
	cmd.ParseFlags(args, false)

	context = &daemon.Context{
		PidFileName: pidFile,
		PidFilePerm: 0644,
		LogFileName: logFile,
		LogFilePerm: 0640,
		WorkDir:     workDir,
	}

	command := cmd.Arg(0)

	daemon.AddCommand(daemon.StringFlag(&command, "stop"), syscall.SIGTERM, termHandler)
	daemon.AddCommand(daemon.StringFlag(&command, "interrupt"), syscall.SIGINT, termHandler)
	daemon.AddCommand(daemon.StringFlag(&command, "quit"), syscall.SIGQUIT, termHandler)
	daemon.AddCommand(daemon.StringFlag(&command, "reload"), syscall.SIGHUP, reloadHandler)

	if command == "status" {
		d, err := context.Search()
		if err == nil {
			err = d.Signal(syscall.Signal(0))
		}
		if err != nil {
			fmt.Println("Daemon is either stopped or inaccessible")
			os.Exit(1)
		} else {
			fmt.Println("Daemon is running")
			os.Exit(0)
		}
		return nil
	}

	if command == "stop" || command == "reload" {
		d, err := context.Search()
		if err != nil {
			return fmt.Errorf("Unable send signal to the daemon: %v", err)
		}
		daemon.SendCommands(d)
		return nil
	}

	if command != "start" {
		return fmt.Errorf("%s: unknown command", command)
	}

	startArgs := cmd.Args()[1:]
	if len(startArgs) == 0 {
		return fmt.Errorf("start: missing command arguments")
	}

	d, err := context.Reborn()
	if err != nil {
		return err
	}
	if d != nil {
		return nil
	}
	defer context.Release()

	logrus.Info("daemon started")
	if err = worker(startArgs); err != nil {
		return err
	}

	err = daemon.ServeSignals()
	if err != nil {
		logrus.Error(err)
	}
	logrus.Info("daemon terminated")
	return nil
}

func worker(args []string) error {
	terminated := make(chan error, 1)

	box := sandbox.New()
	cmd := exec.Command(args[0], args[1:]...)
	cmd.Stdout = os.Stderr
	cmd.Stderr = os.Stderr
	cmd.Env = sandbox.MakeExecEnv(box.Environ())

	if err := cmd.Start(); err != nil {
		return err
	}

	go func() {
		terminated <- cmd.Wait()
	}()

	go func(process *os.Process) {
		defer close(done)

		for {
			select {
			case err := <-terminated:
				context.Release()
				os.Exit(handleTerminate(err))
				return

			case sig := <-stop:
				if err := process.Signal(sig); err != nil {
					process.Kill()
					return
				}

				if sig == syscall.SIGTERM {
					select {
					case <-terminated:
						return
					case <-time.After(30 * time.Second):
						logrus.Errorf("forcibly kill the daemon")
						process.Kill()
						return
					}
				}
			}
		}
	}(cmd.Process)

	return nil
}

func handleTerminate(err error) int {
	if err == nil {
		logrus.Info("Program terminated")
		return 0
	}

	if msg, ok := err.(*exec.ExitError); ok {
		status := msg.Sys().(syscall.WaitStatus)
		if status.Exited() {
			logrus.Infof("Program terminated with exit code %d", status.ExitStatus())
			return status.ExitStatus()
		} else if status.Signaled() {
			logrus.Infof("Program terminated with signal %v", status.Signal())
			return -1
		} else if status.Stopped() {
			logrus.Infof("Program stopped with signal %v", status.StopSignal())
			return -1
		}
	}

	logrus.Infof("Program terminated with error: %v", err)
	return -1
}

func termHandler(sig os.Signal) error {
	logrus.Info("terminating...")
	stop <- sig
	<-done
	return daemon.ErrStop
}

func reloadHandler(sig os.Signal) error {
	logrus.Info("reload daemon...")
	stop <- sig
	return nil
}

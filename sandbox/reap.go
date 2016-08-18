package sandbox

import (
	"fmt"
	"os"
	"os/exec"
	"os/signal"
	"sync"
	"syscall"

	"github.com/Sirupsen/logrus"
)

var waitpid_queue map[int]chan int
var reap_lock sync.Mutex

// Handle death of child (SIGCHLD) messages. Pushes the signal onto the
// notifications channel if there is a waiter.
func sigChildHandler(notifications chan os.Signal) {
	var sigs = make(chan os.Signal, 3)
	signal.Notify(sigs, syscall.SIGCHLD)

	for {
		var sig = <-sigs
		select {
		case notifications <- sig:
			// pulished it
		default:
			// Notifications channel full - drop it to the
			// floor. This ensures we don't fill up the SIGCHLD
			// queue. The reaper just waits for any child
			// process (pid=-1), so we ain't loosing it.
		}
	}
}

// Clean up behind the children.
func Reap() {
	waitpid_queue = make(map[int]chan int)

	var notifications = make(chan os.Signal, 1)
	go sigChildHandler(notifications)

	for {
		<-notifications
		for {
			var status syscall.WaitStatus
			wpid, err := syscall.Wait4(-1, &status, 0, nil)
			if err == syscall.EINTR {
				wpid, err = syscall.Wait4(-1, &status, 0, nil)
			}
			if err == syscall.ECHILD {
				break
			}

			reap_lock.Lock()
			ch := waitpid_queue[wpid]
			if ch != nil {
				delete(waitpid_queue, wpid)
			}
			reap_lock.Unlock()

			var exit_status int
			if status.Exited() {
				exit_status = status.ExitStatus()
			} else {
				exit_status = int(status)
			}

			if ch != nil {
				ch <- exit_status
			} else {
				if status.Exited() {
					logrus.Infof("child process %d terminated with exit status %d", wpid, exit_status)
				} else if status.Signaled() {
					logrus.Infof("child process %d terminated by signal %v", wpid, status.Signal())
				} else {
					logrus.Infof("child process %d terminated with status %d", wpid, status)
				}
			}
		}
	}
}

func RunCommand(cmd *exec.Cmd) error {
	if waitpid_queue == nil {
		return cmd.Run()
	}

	ch := make(chan int)
	if err := startCmd(cmd, ch); err != nil {
		return err
	}

	status := <-ch
	err := cmd.Wait() // need to call Cmd.Wait to cleanup goroutines
	if syserr, ok := err.(*os.SyscallError); ok && syserr.Err == syscall.ECHILD {
		err = nil // this is ok becaue the child has reaped
	}
	if err == nil && status != 0 {
		err = fmt.Errorf("process terminated with status %d", status)
	}
	return err
}

func startCmd(cmd *exec.Cmd, ch chan int) error {
	reap_lock.Lock()
	defer reap_lock.Unlock()

	if err := cmd.Start(); err != nil {
		return err
	} else {
		waitpid_queue[cmd.Process.Pid] = ch
		return nil
	}
}

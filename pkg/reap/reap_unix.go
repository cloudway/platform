// +build !windows

package reap

import (
	"fmt"
	"os"
	"os/exec"
	"os/signal"
	"sync"
	"syscall"
)

type Reaper struct {
	lock      sync.Mutex
	done      chan struct{}
	waitQueue map[int]chan int
}

func New() *Reaper {
	return &Reaper{
		done:      make(chan struct{}),
		waitQueue: make(map[int]chan int),
	}
}

// Handle death of child (SIGCHLD) messages. Pushes the signal onto the
// notifications channel if there is a waiter.
func sigChildHandler(notifications chan os.Signal, done chan struct{}) {
	var sigs = make(chan os.Signal, 3)
	signal.Notify(sigs, syscall.SIGCHLD)

	for {
		select {
		case <-done:
			signal.Stop(sigs)
			return
		case sig := <-sigs:
			select {
			case notifications <- sig:
				// published it
			default:
				// Notifications channel full - drop it to the
				// floor. This ensures we don't fill up the SIGCHLD
				// queue. The reaper just waits for any child
				// process (pid=-1), so we ain't loosing it.
			}
		}
	}
}

// Run is a long-running routine blocks waiting for child processes to exit
// and reaps them, reporting reaped processes to the optional channel.
func (r *Reaper) Run(reapchan chan<- Child) {
	var notifications = make(chan os.Signal, 1)
	go sigChildHandler(notifications, r.done)

	for {
		// Block for an incoming signal that a child has exited
		select {
		case <-notifications:
			// Got a child signal, drop out and reap
			r.reap(reapchan)
		case <-r.done:
			return
		}
	}
}

func (r *Reaper) reap(reapchan chan<- Child) {
	for {
		var status syscall.WaitStatus
		pid, err := syscall.Wait4(-1, &status, syscall.WNOHANG, nil)

		switch err {
		case nil:
			if pid <= 0 {
				return
			}

			// Got a child, clean this up and poll again.
			r.lock.Lock()
			ch := r.waitQueue[pid]
			if ch != nil {
				delete(r.waitQueue, pid)
			}
			r.lock.Unlock()

			if ch != nil {
				// A go routine is waiting for subprocess, send it the exit status.
				var exitCode int
				if status.Exited() {
					exitCode = status.ExitStatus()
				} else {
					exitCode = int(status)
				}
				ch <- exitCode
			} else if reapchan != nil {
				reapchan <- Child{Pid: pid, Status: status}
			}
			continue

		case syscall.ECHILD:
			// No more children, we are done
			return

		case syscall.EINTR:
			// We got interrupted, try again. This likely
			// can't happen since we are calling Wait4 in a
			// non-blocking fashion, but it's good to be
			// complete and handle this case rather than
			// fail.
			continue

		default:
			// We got some other error we didn't expect.
			return
		}
	}
}

// Close stop running the reaper.
func (r *Reaper) Close() {
	close(r.done)
}

// RunCmd execute a command without reaping the subprocess.
func (r *Reaper) RunCmd(cmd *exec.Cmd) error {
	if r == nil {
		return cmd.Run()
	}

	ch := make(chan int, 1)
	if err := r.StartCmd(cmd, ch); err != nil {
		return err
	}
	return r.WaitCmd(cmd, ch)
}

// StartCmd starts a command without reaping the subprocess. A channel must
// be provided to receive notification when subprocess terminated.
func (r *Reaper) StartCmd(cmd *exec.Cmd, ch chan int) error {
	if r == nil {
		return cmd.Start()
	}

	r.lock.Lock()
	defer r.lock.Unlock()

	if err := cmd.Start(); err != nil {
		return err
	}

	r.waitQueue[cmd.Process.Pid] = ch
	return nil
}

// WaitCmd waits on subprocess to finish executing.
func (r *Reaper) WaitCmd(cmd *exec.Cmd, ch chan int) error {
	status := <-ch
	err := cmd.Wait() // need to call Cmd.Wait to cleanup goroutines
	if syserr, ok := err.(*os.SyscallError); ok && syserr.Err == syscall.ECHILD {
		err = nil // this is ok because the child has been reaped
	}
	if err == nil && status != 0 {
		err = fmt.Errorf("process terminated with status %d", status)
	}
	return err
}

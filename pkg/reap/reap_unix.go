// +build !windows

package reap

import (
	"os"
	"os/signal"
	"sync"
	"syscall"
)

type Reaper struct {
	lock sync.RWMutex
	done chan struct{}
}

func New() *Reaper {
	return &Reaper{
		done: make(chan struct{}),
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
	r.lock.Lock()
	defer r.lock.Unlock()

	for {
		var status syscall.WaitStatus
		pid, err := syscall.Wait4(-1, &status, syscall.WNOHANG, nil)

		switch err {
		case nil:
			if pid > 0 {
				// Got a child, clean this up and poll again.
				if reapchan != nil {
					reapchan <- Child{Pid: pid, Status: status}
				}
				continue
			}
			return

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

// Lock to prevent reaping during periods when you know your application
// is waiting for subprocesses to return. You need to use care in order
// to prevent the reaper from stealing your return values from uses of
// packages like Go's exec. We use an RWMutex so that we don't serialize
// all of the application's execution of sub processes with each other,
// but we do serialize them with reaping. The application should get a
// read lock when it wants to do a wait.
func (r *Reaper) Lock() {
	r.lock.RLock()
}

// Unlock reaping when application finished waiting for subprocesses to return.
func (r *Reaper) Unlock() {
	r.lock.RUnlock()
}

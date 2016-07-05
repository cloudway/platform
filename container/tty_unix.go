// +build !windows

package container

import (
    "os"
    "os/signal"
    "syscall"
)

func (t *Tty) monitorResize(fn func (width, height int) error) {
    t.resize(fn)

    sigchan := make(chan os.Signal, 1)
    signal.Notify(sigchan, syscall.SIGWINCH)
    go func() {
        for range sigchan {
            t.resize(fn)
        }
    }()
}

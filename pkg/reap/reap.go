package reap

import "syscall"

type Child struct {
	Pid    int
	Status syscall.WaitStatus
}

// +build windows

package sshd

import "errors"

func Serve(addr string) error {
    return errors.New("SSH server is not supported on Windows")
}
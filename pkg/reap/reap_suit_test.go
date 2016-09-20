// +build !windows

package reap

import (
	"testing"
	"time"

	. "github.com/onsi/ginkgo"
	. "github.com/onsi/gomega"
)

func TestReap(t *testing.T) {
	RegisterFailHandler(Fail)
	RunSpecs(t, "Reap Suite")
}

var (
	reaper   *Reaper
	didExit  chan struct{}
	reapChan chan Child
)

var _ = BeforeSuite(func() {
	reaper = New()
	didExit = make(chan struct{})
	reapChan = make(chan Child, 1)

	go func() {
		reaper.Run(reapChan)
		close(didExit)
	}()

	time.Sleep(1 * time.Second)
})

var _ = AfterSuite(func() {
	reaper.Close()
	Eventually(didExit).Should(BeClosed())
})

// +build !windows

package reap

import (
	"os"
	"os/exec"
	"syscall"
	"time"

	. "github.com/onsi/ginkgo"
	. "github.com/onsi/gomega"
)

var _ = Describe("Reaper", func() {
	Context("when signal spurious SIGCHLD", func() {
		It("should not report anything", func() {
			cmd := exec.Command("sleep", "5")
			Ω(cmd.Start()).Should(Succeed())
			Ω(syscall.Kill(os.Getpid(), syscall.SIGCHLD)).Should(Succeed())
			Consistently(reapChan).ShouldNot(Receive())
		})
	})

	Context("when spawn and kill subprocess", func() {
		It("should reap subprocess", func() {
			cmd := exec.Command("sleep", "5")
			Ω(cmd.Start()).Should(Succeed())
			Ω(cmd.Process.Kill()).Should(Succeed())

			var child Child
			Eventually(reapChan).Should(Receive(&child))
			Ω(child.Pid).Should(Equal(cmd.Process.Pid))
		})
	})

	Context("when spawn and wait subprocess", func() {
		Context("when reaper unlocked", func() {
			It("should fail to wait", func() {
				cmd := exec.Command("true")
				Ω(cmd.Start()).Should(Succeed())
				time.Sleep(1 * time.Second)
				Ω(cmd.Wait()).ShouldNot(Succeed())

				var child Child
				Eventually(reapChan).Should(Receive(&child))
				Ω(child.Pid).Should(Equal(cmd.Process.Pid))
			})
		})

		Context("when reaper locked", func() {
			It("should success to wait", func() {
				cmd := exec.Command("true")
				ch := make(chan int, 1)
				Ω(reaper.StartCmd(cmd, ch)).Should(Succeed())
				time.Sleep(1 * time.Second)
				Ω(reaper.WaitCmd(cmd, ch)).Should(Succeed())
				Consistently(reapChan).ShouldNot(Receive())
			})
		})
	})
})

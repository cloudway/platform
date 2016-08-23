package cmds

import (
	"github.com/Sirupsen/logrus"
	"github.com/cloudway/platform/scm/mock"
	"net"
)

func (cli *CWMan) CmdGitSSH(args ...string) (err error) {
	var addr, repo string

	cmd := cli.Subcmd("git-ssh")
	cmd.StringVar(&addr, []string{"-bind"}, ":7999", "SSH server bind address")
	cmd.StringVar(&repo, []string{"-repo"}, "/var/git", "Root directory of repositories")
	cmd.ParseFlags(args, true)

	stopc := make(chan bool)
	defer close(stopc)

	sshServer := mock.NewSSHServer(repo)

	l, err := net.Listen("tcp", addr)
	if err != nil {
		return err
	}
	sshServer.Accept(l)

	waitChan := make(chan error)
	go sshServer.Wait(waitChan)
	trapSignals(func() {
		sshServer.Close()
		<-stopc // Wait for CmdGitSSH() to return
	})

	serverErr := <-waitChan
	if serverErr != nil {
		logrus.WithError(serverErr).Error("SSH server error")
	}
	logrus.Info("SSH server terminated")
	return nil
}

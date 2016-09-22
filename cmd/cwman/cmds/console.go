package cmds

import (
	"net"

	"github.com/Sirupsen/logrus"
	"github.com/cloudway/platform/broker"
	"github.com/cloudway/platform/console"
)

func (cli *CWMan) CmdConsole(args ...string) (err error) {
	var addr string

	cmd := cli.Subcmd("console")
	cmd.StringVar(&addr, []string{"-bind"}, ":3000", "Console bind address")
	cmd.ParseFlags(args, true)

	stopc := make(chan struct{})
	defer close(stopc)

	br, err := broker.New(cli.DockerClient)
	if err != nil {
		return err
	}

	con, err := console.NewConsole(br)
	if err != nil {
		return err
	}

	l, err := net.Listen("tcp", addr)
	if err != nil {
		return err
	}
	con.Accept(addr, l)

	waitChan := make(chan error)
	go func() {
		waitChan <- con.Serve()
	}()

	trapSignals(func() {
		con.Close()
		<-stopc // wait for CmdConsole() to return
	})

	err = <-waitChan
	if err != nil {
		logrus.WithError(err).Error("Console server error")
	}
	logrus.Info("Console server terminated")
	return nil
}

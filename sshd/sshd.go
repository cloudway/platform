package sshd

import (
	"bytes"
	"context"
	"encoding/binary"
	"errors"
	"fmt"
	"io"
	"io/ioutil"
	"net"
	"os"
	"path/filepath"

	"github.com/Sirupsen/logrus"
	"golang.org/x/crypto/ssh"

	conf "github.com/cloudway/platform/config"
	"github.com/cloudway/platform/container"
	"github.com/cloudway/platform/container/docker"
	"github.com/cloudway/platform/scm"
	"github.com/docker/engine-api/types"
)

func Serve(engine container.Engine, addr string) error {
	cli := engine.(docker.DockerEngine)

	scm, err := scm.New()
	if err != nil {
		return err
	}

	config := &ssh.ServerConfig{
		PublicKeyCallback: func(c ssh.ConnMetadata, key ssh.PublicKey) (*ssh.Permissions, error) {
			return nil, checkPublicKey(cli, scm, c.User(), key)
		},
	}

	// generate SSH host key
	keyPath := filepath.Join(conf.RootDir, "conf", "host_rsa_key")
	if _, err := os.Stat(keyPath); os.IsNotExist(err) {
		os.MkdirAll(filepath.Dir(keyPath), 0750)
		if err = MakeSSHKeyPair(keyPath); err != nil {
			return err
		}
	}

	privateBytes, err := ioutil.ReadFile(keyPath)
	if err != nil {
		return fmt.Errorf("Failed to load private key: %s", err)
	}
	private, err := ssh.ParsePrivateKey(privateBytes)
	if err != nil {
		return fmt.Errorf("Failed to parse private key: %s", err)
	}
	config.AddHostKey(private)

	// once a ServerConfig has been configured, connection can be accepted
	listener, err := net.Listen("tcp", addr)
	if err != nil {
		return fmt.Errorf("Failed to listen on %s: %s", addr, err)
	}

	// accept all connections
	logrus.Infof("Listening on %s", addr)
	for {
		tcpConn, err := listener.Accept()
		if err != nil {
			logrus.WithError(err).Error("Failed to accept incoming connection")
			continue
		}

		// before use, a handshake must be performed on the incoming net.Conn
		sshConn, chans, reqs, err := ssh.NewServerConn(tcpConn, config)
		if err != nil {
			logrus.WithError(err).Debug("Failed to handshake")
			continue
		}

		logrus.Infof("New SSH connection from %s (%s)", sshConn.RemoteAddr(), sshConn.ClientVersion())
		// discard all global out-of-band requests
		go ssh.DiscardRequests(reqs)
		// accept all channels
		go handleChannels(cli, sshConn, chans)
	}
}

func findContainer(cli docker.DockerEngine, userInfo string) (container.Container, error) {
	ctx := context.Background()

	service, name, namespace := container.SplitNames(userInfo)
	if name == "" || namespace == "" {
		return cli.Inspect(ctx, userInfo)
	}

	var cs []container.Container
	if service == "" || service == "*" {
		cs, _ = cli.FindApplications(ctx, name, namespace)
	} else {
		cs, _ = cli.FindService(ctx, name, namespace, service)
	}

	if len(cs) == 0 {
		return nil, fmt.Errorf("Container not found: %s", userInfo)
	} else {
		return cs[0], nil
	}
}

func checkPublicKey(cli docker.DockerEngine, scm scm.SCM, userInfo string, key ssh.PublicKey) error {
	c, err := findContainer(cli, userInfo)
	if err != nil {
		return err
	}

	keys, err := scm.ListKeys(c.Namespace())
	if err != nil {
		return err
	}

	keyBytes := key.Marshal()
	for _, mykey := range keys {
		authKey, _, _, _, keyerr := ssh.ParseAuthorizedKey([]byte(mykey.Text))
		if keyerr == nil && bytes.Compare(keyBytes, authKey.Marshal()) == 0 {
			return nil
		}
	}

	return errors.New("Permission denied")
}

func handleChannels(cli docker.DockerEngine, conn *ssh.ServerConn, chans <-chan ssh.NewChannel) {
	// use the user name to coordinate the container
	container, err := findContainer(cli, conn.User())
	if err != nil {
		logrus.WithError(err).Errorf("Cannot find the container %q", conn.User())
		conn.Close()
		return
	}

	// service the incoming Channel in goroutine
	for newChannel := range chans {
		go handleChannel(newChannel, cli, container)
	}

	logrus.Debug("Channel closed")
}

func handleChannel(newChannel ssh.NewChannel, cli docker.DockerEngine, container container.Container) {
	if newChannel.ChannelType() != "session" {
		newChannel.Reject(ssh.Prohibited, "")
		return
	}

	// at this point, we have the opportunity to reject the client's request
	// for another logical connection.
	channel, requests, err := newChannel.Accept()
	if err != nil {
		logrus.WithError(err).Error("Could not accept channel")
		return
	}

	// session have out-of-band requests such as "shell", "pty-req" and "env"
	go func() {
		var (
			pty    *pty_req
			execId string
			err    error
		)
		for req := range requests {
			logrus.Debugf("Received request %q", req.Type)
			switch req.Type {
			case "pty-req":
				pty = decodePtyReq(req.Payload)
				// responding true (OK) here will let the client know we have
				// a pty ready for input
				req.Reply(true, nil)
			case "shell":
				execId, err = execShell(channel, cli, container, pty)
				req.Reply(err == nil, nil)
			case "exec":
				if cmd, _, ok := decodeString(req.Payload); ok {
					go execCmd(channel, container, string(cmd))
				} else {
					req.Reply(false, nil)
				}
			case "window-change":
				if execId != "" {
					if dims := decodeWindowChange(req.Payload); dims != nil {
						resize := types.ResizeOptions{Width: int(dims.Width), Height: int(dims.Height)}
						cli.ContainerExecResize(context.Background(), execId, resize)
					}
				}
			default:
				req.Reply(false, nil)
			}
		}
	}()
}

func execShell(channel ssh.Channel, cli docker.DockerEngine, c container.Container, pty *pty_req) (execId string, err error) {
	// construct command to run in sandbox, passing TERM environment variable
	cmd := []string{"/usr/bin/cwctl", "sh"}
	if pty != nil {
		cmd = append(cmd, "-e", "TERM="+pty.Term)
	}
	cmd = append(cmd, "cwsh")

	execConfig := types.ExecConfig{
		Tty:          true,
		AttachStdin:  true,
		AttachStdout: true,
		AttachStderr: true,
		Cmd:          cmd,
	}

	ctx := context.Background()

	execResp, err := cli.ContainerExecCreate(ctx, c.ID(), execConfig)
	if err != nil {
		return
	}
	execId = execResp.ID

	resp, err := cli.ContainerExecAttach(ctx, execId, execConfig)
	if err != nil {
		return
	}

	if pty != nil {
		resize := types.ResizeOptions{Width: int(pty.Width), Height: int(pty.Height)}
		cli.ContainerExecResize(ctx, execId, resize)
	}

	go func() {
		defer channel.Close()
		defer resp.Close()

		// pipe session to container and vice-versa
		go func() {
			io.Copy(resp.Conn, channel)
			resp.CloseWrite()
			logrus.Debug("[hijack] End of stdin")
		}()

		io.Copy(channel, resp.Reader)
		logrus.Debug("[hijack] End of stdout")

		// send exit code to ssh client
		exitCode := 0
		inspectResp, err := cli.ContainerExecInspect(ctx, execId)
		if err != nil {
			logrus.WithError(err).Error("Could not inspect exec")
			exitCode = 127
		} else {
			exitCode = inspectResp.ExitCode
		}

		sendExitStatus(channel, exitCode)
		logrus.Debug("Session closed")
	}()

	return
}

func execCmd(channel ssh.Channel, c container.Container, args string) {
	defer channel.Close()

	logrus.Debugf("exec: %s", args)
	err := c.Exec(context.Background(), "", channel, channel, channel.Stderr(), "/usr/bin/cwsh", "-c", args)

	var exitCode int
	if se, ok := err.(container.StatusError); ok {
		exitCode = se.Code
	} else if err != nil {
		fmt.Fprintln(channel.Stderr(), err)
		exitCode = 127
	} else {
		exitCode = 0
	}

	sendExitStatus(channel, exitCode)
	logrus.Debug("Session closed")
}

func sendExitStatus(channel ssh.Channel, code int) {
	payload := make([]byte, 4)
	binary.BigEndian.PutUint32(payload, uint32(code))
	channel.SendRequest("exit-status", false, payload)
}

func decodeString(in []byte) (out, rest []byte, ok bool) {
	if len(in) < 4 {
		return
	}
	length := binary.BigEndian.Uint32(in)
	in = in[4:]
	if uint32(len(in)) < length {
		return
	}
	out = in[:length]
	rest = in[length:]
	ok = true
	return
}

type pty_req struct {
	Term          string
	Width         uint32
	Height        uint32
	WidthInPixel  uint32
	HeightInPixel uint32
	TerminalModes []byte
}

func decodePtyReq(data []byte) *pty_req {
	var req pty_req
	if ssh.Unmarshal(data, &req) == nil {
		return &req
	} else {
		return nil
	}
}

type window_change struct {
	Width         uint32
	Height        uint32
	WidthInPixel  uint32
	HeightInPixel uint32
}

func decodeWindowChange(data []byte) *window_change {
	var req window_change
	if ssh.Unmarshal(data, &req) == nil {
		return &req
	} else {
		return nil
	}
}

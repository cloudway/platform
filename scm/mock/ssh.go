package mock

import (
	"fmt"
	"io/ioutil"
	"net"
	"os"
	"path/filepath"
	"time"
	"strings"

	"github.com/Sirupsen/logrus"
	"golang.org/x/crypto/ssh"

	"github.com/cloudway/platform/sshd"
)

type SSHServer struct {
	repositoryRoot string
	listener       net.Listener
}

func NewSSHServer(repositoryRoot string) *SSHServer {
	return &SSHServer{repositoryRoot: repositoryRoot}
}

func (s *SSHServer) Accept(listener net.Listener) {
	s.listener = listener
}

func (s *SSHServer) Close() error {
	return s.listener.Close()
}

func (s *SSHServer) Wait(waitChan chan error) {
	waitChan <- s.serve()
}

func (s *SSHServer) serve() error {
	defer s.listener.Close()

	config := &ssh.ServerConfig{
		PublicKeyCallback: func(c ssh.ConnMetadata, key ssh.PublicKey) (*ssh.Permissions, error) {
			return nil, s.checkPublicKey(key)
		},
	}

	key, err := s.generateHostKey()
	if err != nil {
		return err
	}
	config.AddHostKey(key)

	var tempDelay time.Duration // how long to sleep on accept failure
	for {
		tcpConn, err := s.listener.Accept()
		if err != nil {
			if ne, ok := err.(net.Error); ok && ne.Temporary() {
				if tempDelay == 0 {
					tempDelay = 5 * time.Millisecond
				} else {
					tempDelay *= 2
				}
				if max := 1 * time.Second; tempDelay > max {
					tempDelay = max
				}
				logrus.Errorf("ssh: Accept error: %s; retrying in %v", err.Error(), tempDelay)
				time.Sleep(tempDelay)
				continue
			}
			if strings.Contains(err.Error(), "use of closed network connection") {
				err = nil
			}
			return err
		}
		tempDelay = 0

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
		go s.handleChannels(sshConn, chans)
	}
}

func (s *SSHServer) generateHostKey() (ssh.Signer, error) {
	keyPath := filepath.Join(s.repositoryRoot, "host_rsa_key")
	if _, err := os.Stat(keyPath); os.IsNotExist(err) {
		os.MkdirAll(filepath.Dir(keyPath), 0750)
		if err = sshd.MakeSSHKeyPair(keyPath); err != nil {
			return nil, err
		}
	}

	privateBytes, err := ioutil.ReadFile(keyPath)
	if err != nil {
		return nil, fmt.Errorf("Failed to load private key: %s", err)
	}

	return ssh.ParsePrivateKey(privateBytes)
}

func (s *SSHServer) checkPublicKey(key ssh.PublicKey) error {
	return nil
}

func (s *SSHServer) handleChannels(conn *ssh.ServerConn, chans <-chan ssh.NewChannel) {
	for newChannel := range chans {
		go s.handleChannel(newChannel)
	}
}

func (s *SSHServer) handleChannel(newChannel ssh.NewChannel) {
	if newChannel.ChannelType() != "session" {
		newChannel.Reject(ssh.Prohibited, "")
		return
	}

	// at this point, we have the opportunity to reject the client's request
	// for another logical connection.
	_, requests, err := newChannel.Accept()
	if err != nil {
		logrus.WithError(err).Error("Could not accept channel")
		return
	}

	for req := range requests {
		req.Reply(false, nil)
	}
}

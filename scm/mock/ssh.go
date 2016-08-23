package mock

import (
	"bytes"
	"encoding/binary"
	"errors"
	"fmt"
	"io/ioutil"
	"net"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"syscall"
	"time"

	"github.com/Sirupsen/logrus"
	"golang.org/x/crypto/ssh"

	"github.com/cloudway/platform/config"
	"github.com/cloudway/platform/scm"
	"github.com/cloudway/platform/sshd"
)

type SSHServer struct {
	repoRoot string
	mockscm  scm.SCM
	listener net.Listener
}

func NewSSHServer(repoRoot string) *SSHServer {
	return &SSHServer{
		repoRoot: repoRoot,
		mockscm:  mockSCM{repoRoot},
	}
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
		PublicKeyCallback: s.checkPublicKey,
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
	keyPath := filepath.Join(config.RootDir, "conf", "host_rsa_key")
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

func (s *SSHServer) checkPublicKey(_ ssh.ConnMetadata, key ssh.PublicKey) (*ssh.Permissions, error) {
	root, err := os.Open(s.repoRoot)
	if err != nil {
		return nil, err
	}
	defer root.Close()

	namespaces, err := root.Readdir(0)
	if err != nil {
		return nil, err
	}

	var permitted []string
	var keyBytes = key.Marshal()

	for _, ns := range namespaces {
		if ns.IsDir() {
			keys, err := s.mockscm.ListKeys(ns.Name())
			if err != nil {
				continue
			}
			for _, mykey := range keys {
				authKey, _, _, _, keyerr := ssh.ParseAuthorizedKey([]byte(mykey.Text))
				if keyerr == nil && bytes.Compare(keyBytes, authKey.Marshal()) == 0 {
					permitted = append(permitted, ns.Name())
					break
				}
			}
		}
	}

	if len(permitted) == 0 {
		return nil, errors.New("Permission denied")
	}

	perms := &ssh.Permissions{
		Extensions: map[string]string{
			"permitted-namespaces": strings.Join(permitted, ","),
		},
	}
	return perms, nil
}

func (s *SSHServer) handleChannels(conn *ssh.ServerConn, chans <-chan ssh.NewChannel) {
	for newChannel := range chans {
		go s.handleChannel(conn, newChannel)
	}
}

func (s *SSHServer) handleChannel(conn *ssh.ServerConn, newChannel ssh.NewChannel) {
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

	for req := range requests {
		switch req.Type {
		case "exec":
			if cmd, _, ok := decodeString(req.Payload); ok {
				logrus.Debugf("Requesting exec command %s", cmd)
				if args, ok := s.parseCommand(string(cmd), conn.Permissions); ok {
					go execCmd(channel, args)
					continue
				}
			}
			fallthrough
		default:
			req.Reply(false, nil)
		}
	}
}

func (s *SSHServer) parseCommand(cmd string, perms *ssh.Permissions) (args []string, ok bool) {
	args = strings.Fields(cmd)
	if len(args) < 2 {
		return
	}
	if args[0] != "git-upload-pack" && args[0] != "git-receive-pack" {
		return
	}

	repo := args[len(args)-1]
	if strings.HasPrefix(repo, "'") && strings.HasSuffix(repo, "'") {
		repo = repo[1 : len(repo)-1]
	}
	if strings.HasPrefix(repo, "/") {
		repo = repo[1:]
	}
	if strings.HasSuffix(repo, ".git") {
		repo = repo[:len(repo)-4]
	}

	parts := strings.Split(repo, "/")
	if len(parts) != 2 {
		return
	}

	namespace, name := parts[0], parts[1]
	if !isNamespacePermitted(perms, namespace) {
		return
	}

	args[len(args)-1] = filepath.Join(s.repoRoot, namespace, name)
	return args, true
}

func isNamespacePermitted(perms *ssh.Permissions, namespace string) bool {
	if perms.Extensions == nil {
		return false
	}
	permitted := strings.Split(perms.Extensions["permitted-namespaces"], ",")
	for _, ns := range permitted {
		if ns == namespace {
			return true
		}
	}
	return false
}

func execCmd(channel ssh.Channel, args []string) {
	defer channel.Close()

	cmd := exec.Command(args[0], args[1:]...)
	cmd.Stdin = channel
	cmd.Stdout = channel
	cmd.Stderr = channel.Stderr()
	err := cmd.Run()

	var exitCode int
	if err != nil {
		if msg, ok := err.(*exec.ExitError); ok {
			exitCode = msg.Sys().(syscall.WaitStatus).ExitStatus()
		} else {
			exitCode = 127 // unknown
		}
	}
	sendExitStatus(channel, exitCode)
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

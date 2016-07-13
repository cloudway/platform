// +build !windows

package sshd

import (
    "fmt"
    "io"
    "io/ioutil"
    "os"
    "os/exec"
    "net"
    "sync"
    "syscall"
    "unsafe"
    "encoding/binary"
    "path/filepath"

    "github.com/Sirupsen/logrus"
    _pty "github.com/kr/pty"
    "golang.org/x/crypto/ssh"
    "github.com/cloudway/platform/container/conf"
)

func Serve(addr string) error {
    // TODO: find and use user's public key
    config := &ssh.ServerConfig{
        NoClientAuth: true,
    }

    // a keypair should be generated with 'ssh-key-gen -t rsa -f ssh/host_rsa_key'
    keyPath := filepath.Join(conf.RootDir, "ssh", "host_rsa_key")
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
            logrus.WithError(err).Error("Failed to handshake")
            continue
        }

        logrus.Infof("New SSH connection from %s (%s)", sshConn.RemoteAddr(), sshConn.ClientVersion())
        // discard all global out-of-band requests
        go ssh.DiscardRequests(reqs)
        // accept all channels
        go handleChannels(chans)
    }
}

func handleChannels(chans <-chan ssh.NewChannel) {
    // service the incoming Channel in goroutine
    for newChannel := range chans {
        go handleChannel(newChannel)
    }
    logrus.Debug("Channel closed")
}

func handleChannel(newChannel ssh.NewChannel) {
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
            pty *os.File
            pty_req, env []byte
            err error
        )
        for req := range requests {
            logrus.Debugf("Received request %q", req.Type)
            switch req.Type {
            case "pty-req":
                pty_req = req.Payload
                // responding true (OK) here will let the client know we have
                // a pty ready for input
                req.Reply(true, nil)
            case "env":
                env = req.Payload
            case "shell":
                pty, err = execShell(channel, pty_req, env)
                req.Reply(err == nil, nil)
            case "exec":
                args := string(req.Payload[4:])
                err = execCmd(channel, env, args)
                req.Reply(err == nil, nil)
            case "window-change":
                if pty != nil {
                    w, h := parseDims(req.Payload)
                    setWinsize(pty.Fd(), w, h)
                }
            default:
                if req.WantReply {
                    req.Reply(false, nil)
                }
            }
        }
    }()
}

func execShell(channel ssh.Channel, pty_req, env []byte) (pty *os.File, err error) {
    // fire up bash for this session
    c := exec.Command("bash")
    c.Env = parseEnv(env)

    // allocate a terminal for this channel
    pty, err = _pty.Start(c)
    if err != nil {
        fmt.Fprintf(channel.Stderr(), "exec failed: %s\r\n", err)
        channel.SendRequest("exit-status", false, []byte{0, 0, 0, 127})
        channel.Close()
        return
    }

    // prepare teardown function
    close := func() {
        err := c.Wait()
        sendExitStatus(channel, err)
        channel.Close()
        logrus.Debug("Session closed")
    }

    // TODO: handle other pty-req request
    if len(pty_req) != 0 {
        termLen := binary.BigEndian.Uint32(pty_req)
        w, h := parseDims(pty_req[4+termLen:])
        setWinsize(pty.Fd(), w, h)
    }

    // pipe session to bash and vice-versa
    var once sync.Once
    go func() {
        io.Copy(channel, pty)
        once.Do(close)
    }()
    go func() {
        io.Copy(pty, channel)
        once.Do(close)
    }()

    return
}

func execCmd(channel ssh.Channel, env []byte, args string) (err error) {
    logrus.Debugf("exec: %s", args)
    c := exec.Command("bash", "-c", args)
    c.Env = parseEnv(env)

    stdout, err := c.StdoutPipe()
    if err != nil {
        logrus.WithError(err).Error("cannot open standard output pipe")
        return err
    }

    stderr, err := c.StderrPipe()
    if err != nil {
        logrus.WithError(err).Error("cannot open standard error pipe")
        return err
    }

    stdin, err := c.StdinPipe()
    if err != nil {
        logrus.WithError(err).Error("cannot open starndard input pipe")
        return err
    }

    if err := c.Start(); err != nil {
        fmt.Fprintf(channel.Stderr(), "exec failed: %s\n", err)
        channel.SendRequest("exit-status", false, []byte{0, 0, 0, 127})
        channel.Close()
        return err
    }

    go func() {
        io.Copy(stdin, channel)
        stdin.Close()
        logrus.Debug("Stdin closed")
    }()

    go io.Copy(channel, stdout)
    go io.Copy(channel.Stderr(), stderr)

    go func() {
        err := c.Wait()
        sendExitStatus(channel, err)
        channel.Close()
        logrus.Debug("Session closed")
    }()

    return nil
}

func sendExitStatus(channel ssh.Channel, err error) {
    payload := make([]byte, 4)
    if exiterr, ok := err.(*exec.ExitError); ok {
        if status, ok := exiterr.Sys().(syscall.WaitStatus); ok {
            var code int
            if status.Exited() {
                code = status.ExitStatus()
            } else if status.Signaled() {
                code = -int(status.Signal())
            }
            binary.BigEndian.PutUint32(payload, uint32(code))
        }
    }
    channel.SendRequest("exit-status", false, payload)
}

func parseEnv(b []byte) []string {
    env := os.Environ()
    for len(b) != 0 {
        keyLen := binary.BigEndian.Uint32(b)
        key    := string(b[4 : 4+keyLen])
        valLen := binary.BigEndian.Uint32(b[4+keyLen:])
        val    := string(b[8+keyLen : 8+keyLen+valLen])
        env     = append(env, key+"="+val)
        b       = b[8+keyLen+valLen:]
    }
    return env
}

// parseDims extracts terminal dimensions (with x height) from the provided buffer.
func parseDims(b []byte) (uint32, uint32) {
    w := binary.BigEndian.Uint32(b)
    h := binary.BigEndian.Uint32(b[4:])
    return w, h
}

// winsize stores the height and width of a terminal
type winsize struct {
    height uint16
    width  uint16
    x      uint16
    y      uint16
}

// setWinsize sets the size of the given pty.
func setWinsize(fd uintptr, w, h uint32) {
    ws := &winsize{width: uint16(w), height: uint16(h)}
    syscall.Syscall(syscall.SYS_IOCTL, fd, uintptr(syscall.TIOCSWINSZ), uintptr(unsafe.Pointer(ws)))
}

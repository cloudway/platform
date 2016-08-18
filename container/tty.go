package container

import (
	"golang.org/x/crypto/ssh/terminal"
	"io"
	"os"
)

// Tty holds the terminal information.
type Tty struct {
	// in holds the input stream and closer (io.ReadCloser) for the client
	in io.ReadCloser
	// out holds the output stream (io.Writer) for the client
	out io.Writer
	// err holds the error stream (io.Writer) for the client
	err io.Writer
	// inFd holds the file descriptor of the client's STDIN (if valid)
	inFd int
	// outFd holds the file descriptor of the client's STDOUT (if valid).
	outFd int
	// isTerminalIn indicates whether the client's STDIN ia a TTY
	isTerminalIn bool
	// isTerminalOut indicates whether the client's STDOUT is a TTY
	isTerminalOut bool
	// state holds the terminal state
	state *terminal.State
}

// Returns a new Tty instance.
func NewTty() *Tty {
	t := &Tty{
		in:  os.Stdin,
		out: os.Stdout,
		err: os.Stderr,
	}

	t.inFd, t.isTerminalIn = getFdInfo(t.in)
	t.outFd, t.isTerminalOut = getFdInfo(t.out)

	return t
}

func getFdInfo(s interface{}) (int, bool) {
	var fd int
	var isTerminal bool
	if file, ok := s.(*os.File); ok {
		fd = int(file.Fd())
		isTerminal = terminal.IsTerminal(fd)
	}
	return fd, isTerminal
}

// Returns true if standard input and output are in terminal mode.
func (t *Tty) isTtyMode() bool {
	return t.isTerminalIn && t.isTerminalOut
}

func (t *Tty) makeRaw() error {
	if t.isTerminalIn && os.Getenv("NORAW") == "" {
		state, err := terminal.MakeRaw(t.inFd)
		if err != nil {
			return err
		}
		t.state = state
	}
	return nil
}

func (t *Tty) restore() {
	if t.state != nil {
		terminal.Restore(t.inFd, t.state)
	}
}

func (t *Tty) getSize() (int, int, error) {
	if !t.isTerminalOut {
		return 0, 0, nil
	} else {
		return terminal.GetSize(t.outFd)
	}
}

func (t *Tty) resize(fn func(w, h int) error) error {
	width, height, err := t.getSize()
	if err != nil {
		return err
	}
	if width == 0 && height == 0 {
		return nil
	}
	return fn(width, height)
}

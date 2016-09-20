package serverlog

import (
	"bytes"
	"encoding/json"
	"fmt"
	"io"
	"io/ioutil"

	"github.com/cloudway/platform/pkg/stdcopy"
)

// Error describes the error that occurred in server. `Code` is an integer
// error code, `Message` is the error message.
type Error struct {
	Code    int    `json:"code,omitempty"`
	Message string `json:"msg,omitempty"`
}

// record represents object generated from server
type record struct {
	Error  *Error      `json:"err,omitempty"`
	Result interface{} `json:"obj,omitempty"`
}

func (e *Error) Error() string {
	return fmt.Sprintf("Error response from server: %s", e.Message)
}

// ServerLog encapsulate multiplexed standard output and standard error streams.
type ServerLog struct {
	stdout io.Writer
	stderr io.Writer
}

// New create a multiplexed server log.
func New(w io.Writer) *ServerLog {
	return &ServerLog{
		stdout: stdcopy.NewWriter(w, stdcopy.Stdout),
		stderr: stdcopy.NewWriter(w, stdcopy.Stderr),
	}
}

// Encap encapsulate two streams.
func Encap(stdout, stderr io.Writer) *ServerLog {
	return &ServerLog{
		stdout: stdout,
		stderr: stderr,
	}
}

// Discard write server logs succeed without doing anything.
var Discard = Encap(ioutil.Discard, ioutil.Discard)

func (l *ServerLog) Stdout() io.Writer {
	if l == nil {
		return nil
	} else {
		return l.stdout
	}
}

func (l *ServerLog) Stderr() io.Writer {
	if l == nil {
		return nil
	} else {
		return l.stderr
	}
}

func (l *ServerLog) Write(p []byte) (n int, err error) {
	if l == nil {
		return len(p), nil
	} else {
		return l.stdout.Write(p)
	}
}

func SendError(w io.Writer, err error) error {
	rec := record{
		Error: &Error{Message: err.Error()},
	}
	out := stdcopy.NewWriter(w, stdcopy.Data)
	return json.NewEncoder(out).Encode(&rec)
}

func SendObject(w io.Writer, obj interface{}) error {
	out := stdcopy.NewWriter(w, stdcopy.Data)
	return json.NewEncoder(out).Encode(&record{Result: obj})
}

func Drain(in io.Reader, dstout, dsterr io.Writer, result interface{}) (err error) {
	data := bytes.NewBuffer(nil)
	_, err = stdcopy.Copy(dstout, dsterr, data, in)
	if err != nil {
		return err
	}

	if data.Len() != 0 {
		rec := record{Result: result}
		err = json.NewDecoder(data).Decode(&rec)
		if err == nil && rec.Error != nil {
			err = rec.Error
		}
	}

	return
}

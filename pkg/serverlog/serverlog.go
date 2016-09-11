package serverlog

import (
	"bytes"
	"encoding/json"
	"fmt"
	"io"

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

package serverlog

import (
	"encoding/json"
	"io"
)

// ServerLog describes log messages generated from server and should display
// in client. It also contains a error message to indicate server failure.
type ServerLog struct {
	Message string       `json:"msg,omitempty"`
	Error   *ServerError `json:"err,omitempty"`
	Object  interface{}  `json:"obj,omitempty"`
}

// ServerError describes the error that occurred in server. `Code` is a integer
// error code, `Message` is the error message.
type ServerError struct {
	Code    int    `json:"code,omitempty"`
	Message string `json:"message,omitempty"`
}

func (e *ServerError) Error() string {
	return e.Message
}

type logWriter struct {
	out io.Writer
	enc *json.Encoder
}

func NewLogWriter(out io.Writer) io.Writer {
	return &logWriter{
		out: out,
		enc: json.NewEncoder(out),
	}
}

func (log *logWriter) Write(p []byte) (n int, err error) {
	stream := ServerLog{Message: string(p)}
	if err = log.enc.Encode(&stream); err != nil {
		return 0, err
	}

	type Flusher interface {
		Flush()
	}

	type ErrFlusher interface {
		Flush() error
	}

	switch b := log.out.(type) {
	case Flusher:
		b.Flush()
	case ErrFlusher:
		err = b.Flush()
	}
	return len(p), err
}

func SendError(w io.Writer, err error) error {
	log := ServerLog{
		Error: &ServerError{
			Message: err.Error(),
		},
	}
	return json.NewEncoder(w).Encode(&log)
}

func SendObject(w io.Writer, obj interface{}) error {
	log := ServerLog{
		Object: obj,
	}
	return json.NewEncoder(w).Encode(&log)
}

func Drain(in io.Reader, out io.Writer, result interface{}) (err error) {
	var dec = json.NewDecoder(in)
	for {
		serverLog := ServerLog{
			Object: result,
		}
		if er := dec.Decode(&serverLog); er != nil {
			if er != io.EOF {
				err = er
			}
			break
		}
		if out != nil && serverLog.Message != "" {
			out.Write([]byte(serverLog.Message))
		}
		if serverLog.Error != nil {
			err = serverLog.Error
		}
	}
	return err
}

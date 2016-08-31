package serverlog

import (
	"encoding/json"
	"github.com/cloudway/platform/api/types"
	"io"
)

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
	stream := types.ServerLog{Message: string(p)}
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

func SendError(w io.Writer, err error) {
	log := types.ServerLog{
		Error: &types.ServerError{
			Message: err.Error(),
		},
	}
	json.NewEncoder(w).Encode(&log)
}

func Drain(in io.Reader, out io.Writer) (err error) {
	var dec = json.NewDecoder(in)
	for {
		var serverLog types.ServerLog
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

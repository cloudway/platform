package stdcopy

import (
	"bufio"
	"encoding/binary"
	"errors"
	"fmt"
	"io"

	"github.com/Sirupsen/logrus"
)

// StdType is the type of standard stream a writer can multiplex to.
type StdType byte

const (
	// Stdin represents standard input stream type.
	Stdin StdType = iota
	// Stdout represents standard output stream type.
	Stdout
	// Stderr represents standard error stream type.
	Stderr
	// Data represents application data stream type.
	Data

	stdWriterPrefixLen = 8
	stdWriterFdIndex   = 0
	stdWriterSizeIndex = 4
)

// stdWriter is wrapper of io.Writer with extra customized info.
type stdWriter struct {
	io.Writer
	prefix byte
}

type flusher interface {
	Flush()
}

type errFlusher interface {
	Flush() error
}

// Write sends the buffer to the underneath writer.
// It inserts the prefix header before the buffer,
// so stdcopy.Copy knows where to multiplex the output.
// It makes stdWriter to implement io.Writer.
func (w *stdWriter) Write(p []byte) (n int, err error) {
	if w == nil || w.Writer == nil {
		return 0, errors.New("Writer not instantiated")
	}
	if len(p) == 0 {
		return 0, nil
	}

	const chunkSize = 0x1000 // 4096
	var (
		buf      [chunkSize]byte
		size     = len(p)
		copySize int
	)

	// fill in header
	buf[stdWriterFdIndex] = w.prefix
	binary.BigEndian.PutUint32(buf[stdWriterSizeIndex:], uint32(size))

	// write first chunk
	if copySize = chunkSize - stdWriterPrefixLen; copySize > size {
		copySize = size
	}
	copy(buf[stdWriterPrefixLen:], p[:copySize])
	n, err = w.Writer.Write(buf[:copySize+stdWriterPrefixLen])
	n -= stdWriterPrefixLen
	if n < 0 {
		n = 0
	}

	// write remaining bytes
	if err == nil && copySize < size {
		var n2 int
		n2, err = w.Writer.Write(p[copySize:])
		n += n2
	}

	if err == nil {
		switch b := w.Writer.(type) {
		case flusher:
			b.Flush()
		case errFlusher:
			err = b.Flush()
		}
	}

	return
}

// NewWriter instantiates a new Writer.
// Everything written to it will be encapsulated using a custom format,
// and written to the underlying `w` stream.
// This allows multiple write streams (e.g. stdout and stderr) to be muxed
// into a single connection.
// `t` indicates the id of the stream to encapsulate.
// It can be stdcopy.Stdin, stdcopy.Stdout, stdcopy.Stderr.
func NewWriter(w io.Writer, t StdType) io.Writer {
	return &stdWriter{
		Writer: w,
		prefix: byte(t),
	}
}

// Copy is a modified version of io.Copy.
//
// Copy will demultiplex `src`, assuming that it contains two streams,
// previously multiplexed together using a stdWriter instance.
// As it reads from `src`, Copy will write to `dstout` and `dsterr`.
//
// Copy will read until it hits EOF on `src`. It will then return a nil error.
// In other words: if `err` is non nil, it indicates a real underlying error.
//
// `written` will hold the total number of bytes written to `dstout` and `dsterr`.
func Copy(dstout, dsterr, data io.Writer, src io.Reader) (written int64, err error) {
	var (
		rd     = bufio.NewReader(src)
		wr     io.Writer
		header [stdWriterPrefixLen]byte
		er, ew error
	)

	for {
		// Make sure we have at least a full header
		_, er = io.ReadFull(rd, header[:])
		if er == io.EOF {
			return written, nil
		}
		if er != nil {
			logrus.Debugf("Error reading header: %s", er)
			return written, er
		}

		// Check the first byte to know where to write
		switch StdType(header[stdWriterFdIndex]) {
		case Stdin:
			fallthrough
		case Stdout:
			wr = dstout
		case Stderr:
			wr = dsterr
		case Data:
			wr = data
		default:
			logrus.Debugf("Error selecting output fd: (%d)", header[stdWriterFdIndex])
			return written, fmt.Errorf("Unrecognized input header: %d", header[stdWriterFdIndex])
		}

		// Retrieve the size of the frame
		frameSize := int(binary.BigEndian.Uint32(header[stdWriterSizeIndex : stdWriterSizeIndex+4]))
		logrus.Debugf("framesize: %d", frameSize)

		// Write to output stream
		if wr != nil {
			var nw int64
			nw, ew = io.CopyN(wr, rd, int64(frameSize))
			written += nw
		} else {
			var nw int
			nw, ew = rd.Discard(frameSize)
			written += int64(nw)
		}
		if ew != nil {
			logrus.Debugf("Error writting frame: %s", ew)
			return written, ew
		}
	}
}

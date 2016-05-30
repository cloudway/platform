package container

import (
    "io"
    "fmt"
    "encoding/binary"
)

// StdType is the type of standard stream
// a writer can multiplex to.
type StdType byte

const (
    // Stdin represents standard input stream type.
    Stdin StdType = iota
    // Stdout represents standard output stream type.
    Stdout
    // Stderr represents standard error stream type.
    Stderr

    stdWriterPrefixLen = 8
    stdWriterFdIndex   = 0
    stdWriterSizeIndex = 4

    startingBufLen = 32*1024 + stdWriterPrefixLen + 1
)

// stdCopy is a modified version of io.Copy
//
// stdCopy will demultiplex `src`, assuming that it contains two streams,
// previously multiplexed together using a StdWriter instance.
// As it reads from `src`, stdCopy will write to `dstout` and `dsterr`.
//
// stdCopy will read until it hits EOF on `src`. It will then return a nil error.
// In other words: if `err` is non nil, it indicates a real underlying error.
//
// `written` will hold the total number of bytes written to `dstout` and `dsterr`.
func stdCopy(dstout, dsterr io.Writer, src io.Reader) (written int64, err error) {
    var (
        buf         = make([]byte, startingBufLen)
        bufLen      = len(buf)
        nr, nw      int
        er, ew      error
        out         io.Writer
        frameSize   int
    )

    for {
        // Make sure we have at least a full header
        for nr < stdWriterPrefixLen {
            var nr2 int
            nr2, er = src.Read(buf[nr:])
            nr += nr2
            if er == io.EOF {
                if nr < stdWriterPrefixLen {
                    return written, nil
                }
                break
            }
            if er != nil {
                return 0, er
            }
        }

        // Check the first byte to know where to write
        switch StdType(buf[stdWriterFdIndex]) {
        case Stdin:
            fallthrough
        case Stdout:
            // Write on stdout
            out = dstout
        case Stderr:
            // Write on stderr
            out = dsterr
        default:
            return 0, fmt.Errorf("Unrecognized input header: %d", buf[stdWriterFdIndex])
        }

        // Retrieve the size of the frame
        frameSize = int(binary.BigEndian.Uint32(buf[stdWriterSizeIndex : stdWriterSizeIndex+4]))

        // Check if the buffer is big enough to read the frame.
        // Extend it if necessary.
        if frameSize+stdWriterPrefixLen > bufLen {
            buf = append(buf, make([]byte, frameSize+stdWriterPrefixLen-bufLen+1)...)
            bufLen = len(buf)
        }

        // While the amount of bytes read is less than the size of the frame + header, we keep reading
        for nr < frameSize+stdWriterPrefixLen {
            var nr2 int
            nr2, er = src.Read(buf[nr:])
            nr += nr2
            if er == io.EOF {
                if nr < frameSize+stdWriterPrefixLen {
                    return written, nil
                }
                break
            }
            if er != nil {
                return 0, er
            }
        }

        // Write the retrieved frame (without header)
        if out != nil {
            nw, ew = out.Write(buf[stdWriterPrefixLen : frameSize+stdWriterPrefixLen])
            if ew != nil {
                return 0, ew
            }
            // If the frame has not been fully written: error
            if nw != frameSize {
                return 0, io.ErrShortWrite
            }
            written += int64(nw)
        }

        // Move the rest of the buffer to the begining
        copy(buf, buf[frameSize+stdWriterPrefixLen:])
        // Move the index
        nr -= frameSize + stdWriterPrefixLen
    }
}

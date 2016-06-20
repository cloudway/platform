package rest

import (
    "errors"
    "fmt"
)

// ErrConnectionFailed is an error raised when the connection between the client and server failed.
var ErrConnectionFailed = errors.New("Cannot connect to the server.")

// A error that contains error messages returned from API server.
type ServerError []byte

func (se ServerError) Error() string {
    return fmt.Sprintf("Error response from server: %s", string(se))
}

func (se ServerError) RawError() []byte {
    return []byte(se)
}

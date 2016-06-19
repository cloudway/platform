package rest

import "errors"

// ErrConnectionFailed is an error raised when the connection between the client and server failed.
var ErrConnectionFailed = errors.New("Cannot connect to the server.")

// unauthorizedError represents an authorization error in a API server.
type unauthorizedError struct {
    cause error
}

// Error returns a string representation of an unauthorizedError
func (u unauthorizedError) Error() string {
    return u.cause.Error()
}

// IsUnauthorizedError returns true if the error is caused when a server authentication fails.
func IsUnauthorizedError(err error) bool {
    _, ok := err.(unauthorizedError)
    return ok
}

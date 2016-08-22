package middleware

import "github.com/cloudway/platform/api/server/httputils"

// Middleware is an interface to allow the use of ordinary functions as API filters.
// Any struct that has the appropriate signature can be registered as a middleware.
type Middleware interface {
	WrapHandler(httputils.APIFunc) httputils.APIFunc
}

// key is an unexported type for keys defined in this package.
// This prevents collisions with keys defined in other packages.
type userkey int

// UserKey is the key for user.User values in Contexts.
const UserKey userkey = 0

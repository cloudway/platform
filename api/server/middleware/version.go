package middleware

import (
    "net/http"
    "golang.org/x/net/context"
    "strings"
    "strconv"
    "fmt"
    "github.com/cloudway/platform/api/server/httputils"
)

type badRequestError struct {
    error
}

func (badRequestError) HTTPErrorStatusCode() int {
    return http.StatusBadRequest
}

// VersionMiddleware is a middleware that validates the client and server versions.
type VersionMiddleware struct {
    serverVersion  string
    minVersion     string
    dockerVersion  string
}

// NewVersionMiddleware creates a new VersionMiddleware with the default versions
func NewVersionMiddleware(s, m, d string) VersionMiddleware {
    return VersionMiddleware{
        serverVersion:  s,
        minVersion:     m,
        dockerVersion:  d,
    }
}

// WrapHandler returns a new handler function wrapping the previous one in the request chain
func (v VersionMiddleware) WrapHandler(handler httputils.APIFunc) httputils.APIFunc {
    return func(ctx context.Context, w http.ResponseWriter, r *http.Request, vars map[string]string) error {
        apiVersion := vars["version"]
        if apiVersion == "" {
            apiVersion = v.serverVersion
        }

        if compareVersions(apiVersion, v.serverVersion) > 0 {
            return badRequestError{
                fmt.Errorf("client is newer than server (client API version: %s, server API version: %s)",
                           apiVersion, v.serverVersion),
            }
        }
        if compareVersions(apiVersion, v.minVersion) < 0 {
            return badRequestError{
                fmt.Errorf("client version %s is too old. Minimum supported API version is %s, " +
                           "please upgrade your client to a newer version", apiVersion, v.minVersion),
            }
        }

        header := fmt.Sprintf("Cloudway-API/%s Docker/%s", v.serverVersion, v.dockerVersion)
        w.Header().Set("Server", header)
        ctx = context.WithValue(ctx, httputils.APIVersionKey, apiVersion)
        return handler(ctx, w, r, vars)
    }
}

func compareVersions(v1, v2 string) int {
    currTab := strings.Split(v1, ".")
    otherTab := strings.Split(v2, ".")

    max := len(currTab)
    if len(otherTab) > max {
        max = len(otherTab)
    }

    for i := 0; i < max; i++ {
        var currInt, otherInt int
        if len(currTab) > i {
            currInt, _ = strconv.Atoi(currTab[i])
        }
        if len(otherTab) > i {
            otherInt, _ = strconv.Atoi(otherTab[i])
        }
        if currInt > otherInt {
            return 1
        }
        if otherInt > currInt {
            return -1
        }
    }
    return 0
}
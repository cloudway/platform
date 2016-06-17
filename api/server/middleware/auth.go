package middleware

import (
    "regexp"
    "net/http"
    "golang.org/x/net/context"
    "github.com/Sirupsen/logrus"
    "github.com/cloudway/platform/api/server/httputils"
    "github.com/cloudway/platform/api/server/auth"
)

type authMiddleware struct {
    auth *auth.Authenticator
}

func NewAuthMiddleware(auth *auth.Authenticator) authMiddleware {
    return authMiddleware{auth}
}

var noAuthPattern = regexp.MustCompile("^(/v[0-9.]+)?/(version|auth)$")

func (a authMiddleware) WrapHandler(handler httputils.APIFunc) httputils.APIFunc {
    return func(ctx context.Context, w http.ResponseWriter, r *http.Request, vars map[string]string) error {
        if noAuthPattern.MatchString(r.URL.Path) {
            return handler(ctx, w, r, vars)
        }

        user, err := a.auth.Verify(w, r)
        if err != nil {
            w.WriteHeader(http.StatusUnauthorized)
            return nil
        }

        logrus.Debugf("Logged in user: %v", user)
        ctx = context.WithValue(ctx, "login-user", user)
        return handler(ctx, w, r, vars)
    }
}

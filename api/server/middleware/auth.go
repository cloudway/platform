package middleware

import (
    "regexp"
    "net/http"
    "golang.org/x/net/context"
    "github.com/Sirupsen/logrus"
    "github.com/cloudway/platform/api/server/httputils"
    "github.com/cloudway/platform/api/server/runtime"
)

type authMiddleware struct {
    *runtime.Runtime
    noAuthPattern *regexp.Regexp
}

func NewAuthMiddleware(rt *runtime.Runtime, contextRoot string) authMiddleware {
    pattern := regexp.MustCompile("^" + contextRoot + "(/v[0-9.]+)?/(version|auth)$")
    return authMiddleware{rt, pattern}
}

func (m authMiddleware) WrapHandler(handler httputils.APIFunc) httputils.APIFunc {
    return func(ctx context.Context, w http.ResponseWriter, r *http.Request, vars map[string]string) error {
        if m.noAuthPattern.MatchString(r.URL.Path) {
            return handler(ctx, w, r, vars)
        }

        user, err := m.Authz.Verify(w, r)
        if err != nil {
            w.WriteHeader(http.StatusUnauthorized)
            return nil
        }

        logrus.Debugf("Logged in user: %v", user)
        vars["user"] = user.Name
        vars["namespace"] = user.Namespace
        return handler(ctx, w, r, vars)
    }
}

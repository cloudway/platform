package system

import (
    osruntime "runtime"
    "net/http"
    "golang.org/x/net/context"
    "github.com/cloudway/platform/api"
    "github.com/cloudway/platform/api/types"
    "github.com/cloudway/platform/api/server/router"
    "github.com/cloudway/platform/api/server/httputils"
    "github.com/cloudway/platform/api/server/runtime"
)

type systemRouter struct {
    *runtime.Runtime
    routes []router.Route
}

func NewRouter(rt *runtime.Runtime) router.Router {
    r := &systemRouter{Runtime: rt}

    r.routes = []router.Route{
        router.NewGetRoute("/version", r.getVersion),
        router.NewPostRoute("/auth", r.postAuth),
    }

    return r
}

func (s *systemRouter) Routes() []router.Route {
    return s.routes
}

func (s *systemRouter) getVersion(ctx context.Context, w http.ResponseWriter, r *http.Request, vars map[string]string) error {
    info, err := s.ServerVersion(ctx)
    if err != nil {
        return err
    }

    v := types.Version{
        Version:        api.Version,
        DockerVersion:  info.Version,
        Os:             osruntime.GOOS,
        Arch:           osruntime.GOARCH,
    }

    return httputils.WriteJSON(w, http.StatusOK, v)
}

func (s *systemRouter) postAuth(ctx context.Context, w http.ResponseWriter, r *http.Request, vars map[string]string) error {
    username, password, ok := r.BasicAuth()
    if !ok {
        w.WriteHeader(http.StatusUnauthorized)
        return nil
    }

    _, token, err := s.Authz.Authenticate(username, password)
    if err != nil {
        w.WriteHeader(http.StatusForbidden)
        return nil
    }

    w.WriteHeader(http.StatusOK)
    _, err = w.Write([]byte(token))
    return err
}

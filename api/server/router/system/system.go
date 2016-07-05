package system

import (
    osruntime "runtime"
    "net/http"
    "github.com/Sirupsen/logrus"
    "golang.org/x/net/context"
    "github.com/cloudway/platform/api"
    "github.com/cloudway/platform/api/types"
    "github.com/cloudway/platform/api/server/router"
    "github.com/cloudway/platform/api/server/httputils"
    "github.com/cloudway/platform/broker"
)

type systemRouter struct {
    *broker.Broker
    routes []router.Route
}

func NewRouter(broker *broker.Broker) router.Router {
    r := &systemRouter{Broker: broker}

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
        GitCommit:      api.GitCommit,
        BuildTime:      api.BuildTime,
        DockerVersion:  info.Version,
        Os:             osruntime.GOOS,
        Arch:           osruntime.GOARCH,
    }

    return httputils.WriteJSON(w, http.StatusOK, v)
}

func (s *systemRouter) postAuth(ctx context.Context, w http.ResponseWriter, r *http.Request, vars map[string]string) error {
    username, password, ok := r.BasicAuth()
    if !ok {
        http.Error(w, "Requires username and password", http.StatusUnauthorized)
        return nil
    }

    _, token, err := s.Authz.Authenticate(username, password)
    if err != nil {
        logrus.WithField("username", username).WithError(err).Debug("Login failed")
        http.Error(w, "Login failed", http.StatusUnauthorized)
        return nil
    }

    return httputils.WriteJSON(w, http.StatusOK, map[string]interface{}{
        "Token": token,
    })
}

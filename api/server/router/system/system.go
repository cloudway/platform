package system

import (
    "runtime"
    "net/http"
    "golang.org/x/net/context"
    "github.com/cloudway/platform/api/server/router"
    "github.com/cloudway/platform/api/types"
    "github.com/cloudway/platform/api"
    "github.com/cloudway/platform/api/server/httputils"
    "github.com/cloudway/platform/container"
)

type systemRouter struct {
    container.DockerClient
    routes []router.Route
}

func NewRouter(client container.DockerClient) router.Router {
    r := &systemRouter{DockerClient: client}

    r.routes = []router.Route{
        router.NewGetRoute("/ping", handlePing),
        router.NewGetRoute("/version", r.getVersion),
    }

    return r
}

func (s *systemRouter) Routes() []router.Route {
    return s.routes
}

func handlePing(ctx context.Context, w http.ResponseWriter, r *http.Request, vars map[string]string) error {
    _, err := w.Write([]byte{'O', 'K'})
    return err
}

func (s *systemRouter) getVersion(ctx context.Context, w http.ResponseWriter, r *http.Request, vars map[string]string) error {
    info, err := s.ServerVersion(ctx)
    if err != nil {
        return err
    }

    v := types.Version{
        Version:        api.Version,
        DockerVersion:  info.Version,
        Os:             runtime.GOOS,
        Arch:           runtime.GOARCH,
    }

    return httputils.WriteJSON(w, http.StatusOK, v)
}

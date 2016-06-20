package plugins

import (
    "net/http"
    "golang.org/x/net/context"
    "github.com/cloudway/platform/api/server/runtime"
    "github.com/cloudway/platform/api/server/router"
    "github.com/cloudway/platform/api/server/httputils"
)

type pluginsRouter struct {
    *runtime.Runtime
    routes []router.Route
}

func NewRouter(rt *runtime.Runtime) router.Router {
    r := &pluginsRouter{Runtime: rt}

    r.routes = []router.Route{
        router.NewGetRoute("/plugins/{tag:.*}", r.getPluginInfo),
    }

    return r
}

func (pr *pluginsRouter) Routes() []router.Route {
    return pr.routes
}

func (pr *pluginsRouter) getPluginInfo(ctx context.Context, w http.ResponseWriter, r *http.Request, vars map[string]string) error {
    meta, err := pr.Hub.GetPluginInfo(vars["tag"])
    if err != nil {
        return err
    }
    meta.Path = ""
    return httputils.WriteJSON(w, http.StatusOK, meta)
}

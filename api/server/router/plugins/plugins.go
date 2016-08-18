package plugins

import (
	"golang.org/x/net/context"
	"net/http"

	"github.com/cloudway/platform/api/server/httputils"
	"github.com/cloudway/platform/api/server/router"
	"github.com/cloudway/platform/broker"
)

type pluginsRouter struct {
	*broker.Broker
	routes []router.Route
}

func NewRouter(broker *broker.Broker) router.Router {
	r := &pluginsRouter{Broker: broker}

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

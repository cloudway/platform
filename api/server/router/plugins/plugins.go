package plugins

import (
	"golang.org/x/net/context"
	"net/http"
	"strings"

	"github.com/cloudway/platform/api/server/httputils"
	"github.com/cloudway/platform/api/server/router"
	"github.com/cloudway/platform/broker"
	"github.com/cloudway/platform/pkg/manifest"
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
	if err := httputils.ParseForm(r); err != nil {
		return err
	}

	var (
		plugins   []*manifest.Plugin
		tag       = strings.TrimSpace(vars["tag"])
		namespace = ""
		category  = manifest.Category(r.FormValue("category"))
	)

	if strings.HasSuffix(tag, "/") {
		namespace = tag[:len(tag)-1]
		tag = ""
	}

	if tag == "" {
		plugins = pr.Hub.ListPlugins(namespace, category)
	} else {
		plugin, err := pr.Hub.GetPluginInfo(tag)
		if err != nil {
			return err
		}
		plugins = []*manifest.Plugin{plugin}
	}

	for _, p := range plugins {
		p.Path = ""
	}

	return httputils.WriteJSON(w, http.StatusOK, plugins)
}

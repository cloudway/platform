package plugins

import (
	"net/http"

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
		router.NewGetRoute("/plugins/", r.list),
		router.NewGetRoute("/plugins/{tag:.*}", r.info),
		router.NewPostRoute("/plugins/", r.create),
		router.NewDeleteRoute("/plugins/{tag:.*}", r.remove),
	}

	return r
}

func (pr *pluginsRouter) Routes() []router.Route {
	return pr.routes
}

func (pr *pluginsRouter) NewUserBroker(r *http.Request) *broker.UserBroker {
	ctx := r.Context()
	user := httputils.UserFromContext(ctx)
	return pr.Broker.NewUserBroker(user, ctx)
}

func (pr *pluginsRouter) list(w http.ResponseWriter, r *http.Request, vars map[string]string) error {
	if err := httputils.ParseForm(r); err != nil {
		return err
	}

	category := manifest.Category(r.FormValue("category"))
	_, userDefined := r.Form["user"]

	br := pr.NewUserBroker(r)

	var plugins []*manifest.Plugin
	if userDefined {
		plugins = br.GetUserPlugins(category)
	} else {
		plugins = br.GetInstalledPlugins(category)
	}
	if plugins == nil {
		plugins = make([]*manifest.Plugin, 0)
	}
	return httputils.WriteJSON(w, http.StatusOK, plugins)
}

func (pr *pluginsRouter) info(w http.ResponseWriter, r *http.Request, vars map[string]string) error {
	plugin, err := pr.NewUserBroker(r).GetPluginInfo(vars["tag"])
	if err != nil {
		return err
	}
	return httputils.WriteJSON(w, http.StatusOK, plugin)
}

func (pr *pluginsRouter) create(w http.ResponseWriter, r *http.Request, vars map[string]string) error {
	return pr.NewUserBroker(r).InstallPlugin(r.Body)
}

func (pr *pluginsRouter) remove(w http.ResponseWriter, r *http.Request, vars map[string]string) error {
	return pr.NewUserBroker(r).RemovePlugin(vars["tag"])
}

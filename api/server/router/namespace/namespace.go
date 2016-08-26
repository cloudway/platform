package namespace

import (
	"net/http"

	"github.com/cloudway/platform/api/server/httputils"
	"github.com/cloudway/platform/api/server/router"
	"github.com/cloudway/platform/broker"
	"golang.org/x/net/context"
)

type namespaceRouter struct {
	*broker.Broker
	routes []router.Route
}

func NewRouter(broker *broker.Broker) router.Router {
	r := &namespaceRouter{Broker: broker}

	r.routes = []router.Route{
		router.NewGetRoute("/namespace", r.get),
		router.NewPostRoute("/namespace", r.set),
		router.NewDeleteRoute("/namespace", r.delete),
	}

	return r
}

func (nr *namespaceRouter) Routes() []router.Route {
	return nr.routes
}

func (nr *namespaceRouter) get(ctx context.Context, w http.ResponseWriter, r *http.Request, vars map[string]string) error {
	user := httputils.UserFromContext(ctx)
	br := nr.NewUserBroker(user, ctx)
	if err := br.Refresh(); err != nil {
		return err
	}
	return httputils.WriteJSON(w, http.StatusOK, map[string]interface{}{
		"Namespace": user.Namespace,
	})
}

func (nr *namespaceRouter) set(ctx context.Context, w http.ResponseWriter, r *http.Request, vars map[string]string) error {
	if err := httputils.ParseForm(r); err != nil {
		return err
	}
	user := httputils.UserFromContext(ctx)
	br := nr.NewUserBroker(user, ctx)
	return br.CreateNamespace(r.FormValue("namespace"))
}

func (nr *namespaceRouter) delete(ctx context.Context, w http.ResponseWriter, r *http.Request, vars map[string]string) error {
	if err := httputils.ParseForm(r); err != nil {
		return err
	}
	user := httputils.UserFromContext(ctx)
	br := nr.NewUserBroker(user, ctx)
	_, force := r.Form["force"]
	return br.RemoveNamespace(force)
}

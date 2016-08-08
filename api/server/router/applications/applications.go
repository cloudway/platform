package applications

import (
    "fmt"
    "strings"
    "net/url"
    "net/http"

    "golang.org/x/net/context"
    "github.com/cloudway/platform/broker"
    "github.com/cloudway/platform/api/server/router"
    "github.com/cloudway/platform/auth/userdb"
    "github.com/cloudway/platform/api/server/httputils"
    "github.com/cloudway/platform/pkg/manifest"
    "github.com/cloudway/platform/config"
    "github.com/cloudway/platform/config/defaults"
)

type applicationsRouter struct {
    *broker.Broker
    routes []router.Route
}

func NewRouter(broker *broker.Broker) router.Router {
    r := &applicationsRouter{Broker: broker}

    r.routes = []router.Route{
        router.NewGetRoute("/applications/", r.getApplications),
        router.NewGetRoute("/applications/{name:.*}", r.getApplicationInfo),
    }

    return r
}

func (ar *applicationsRouter) Routes() []router.Route {
    return ar.routes
}

func (ar *applicationsRouter) currentUser(vars map[string]string) (*userdb.BasicUser, error) {
    var user userdb.BasicUser
    err := ar.Users.Find(vars["user"], &user)
    return &user, err
}

func (ar *applicationsRouter) getApplications(ctx context.Context, w http.ResponseWriter, r *http.Request, vars map[string]string) error {
    user, err := ar.currentUser(vars)
    if err != nil {
        return err
    }

    var apps []string
    for name, _ := range user.Applications {
        apps = append(apps, name)
    }

    return httputils.WriteJSON(w, http.StatusOK, apps)
}

func (ar *applicationsRouter) getApplicationInfo(ctx context.Context, w http.ResponseWriter, r *http.Request, vars map[string]string) error {
    user, err := ar.currentUser(vars)
    if err != nil {
        return err
    }

    name := vars["name"]
    app  := user.Applications[name]
    if app == nil {
        return httputils.NewStatusError(http.StatusNotFound)
    }

    info := manifest.ApplicationInfo{
        Name:       name,
        Namespace:  user.Namespace,
        CreatedAt:  app.CreatedAt,
    }

    base, err := url.Parse(config.GetOrDefault("console.url", "http://api."+defaults.Domain()))
    if err != nil {
        return err
    }

    host, port := base.Host, ""
    if i := strings.IndexRune(host, ':'); i != -1 {
        host, port = host[:i], host[i:]
    }
    info.URL = fmt.Sprintf("%s://%s-%s.%s%s", base.Scheme, name, user.Namespace, defaults.Domain(), port)
    info.SSHURL = "ssh://"+name+"-"+user.Namespace+"@"+host+":2200" // FIXME port

    cloneURL := config.Get("scm.clone_url")
    if cloneURL != "" {
        cloneURL = strings.Replace(cloneURL, "<namespace>", user.Namespace, -1)
        cloneURL = strings.Replace(cloneURL, "<repo>", name, -1)
        info.CloneURL = cloneURL
    }

    for _, tag := range app.Plugins {
        p, err := ar.Hub.GetPluginInfo(tag)
        if err == nil {
            p.Path = ""
            if p.Category.IsFramework() {
                info.Framework = p
            } else {
                info.Services = append(info.Services, p)
            }
        }
    }

    return httputils.WriteJSON(w, http.StatusOK, &info)
}

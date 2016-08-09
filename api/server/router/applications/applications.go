package applications

import (
    "fmt"
    "strings"
    "net/url"
    "net/http"
    "regexp"
    "encoding/json"

    "golang.org/x/net/context"
    "github.com/cloudway/platform/broker"
    "github.com/cloudway/platform/api/server/router"
    "github.com/cloudway/platform/auth/userdb"
    "github.com/cloudway/platform/api/server/httputils"
    "github.com/cloudway/platform/pkg/manifest"
    "github.com/cloudway/platform/config"
    "github.com/cloudway/platform/config/defaults"
    "github.com/cloudway/platform/container"
)

type applicationsRouter struct {
    *broker.Broker
    routes []router.Route
}

func NewRouter(broker *broker.Broker) router.Router {
    r := &applicationsRouter{Broker: broker}

    r.routes = []router.Route{
        router.NewGetRoute("/applications/", r.list),
        router.NewGetRoute("/applications/{name:.*}", r.info),
        router.NewPostRoute("/applications/", r.create),
        router.NewDeleteRoute("/applications/{name:.*}", r.delete),
        router.NewPostRoute("/applications/{name:.*}/start", r.start),
        router.NewPostRoute("/applications/{name:.*}/stop", r.stop),
        router.NewPostRoute("/applications/{name:.*}/restart", r.restart),
        router.NewPostRoute("/applications/{name:.*}/deploy", r.deploy),
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

func (ar *applicationsRouter) list(ctx context.Context, w http.ResponseWriter, r *http.Request, vars map[string]string) error {
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

func (ar *applicationsRouter) info(ctx context.Context, w http.ResponseWriter, r *http.Request, vars map[string]string) error {
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

var namePattern = regexp.MustCompile("^[a-z][a-z_0-9]*$")

func (ar *applicationsRouter) create(ctx context.Context, w http.ResponseWriter, r *http.Request, vars map[string]string) error {
    if err := httputils.ParseForm(r); err != nil {
        return err
    }
    if err := httputils.CheckForJSON(r); err != nil {
        return err
    }

    user, err := ar.currentUser(vars)
    if err != nil {
        return err
    }

    var req manifest.CreateApplication
    if err := json.NewDecoder(r.Body).Decode(&req); err != nil {
        return err
    }

    opts := container.CreateOptions{
        Name:    req.Name,
        Repo:    req.Repo,
        Scaling: 1,
    }

    if !namePattern.MatchString(opts.Name) {
        msg := "The application name can only contains lower case letters, digits or underscores."
        http.Error(w, msg, http.StatusBadRequest)
        return nil
    }

    if req.Framework == "" {
        msg := "The application framework cannot be empty."
        http.Error(w, msg, http.StatusBadRequest)
        return nil
    }

    tags := append([]string{req.Framework}, req.Services...)
    br := ar.NewUserBroker(user)

    cs, err := br.CreateApplication(opts, tags)
    if err != nil {
        return err
    }

    if err = br.StartContainers(cs); err != nil {
        return err
    }

    w.WriteHeader(http.StatusCreated)
    return nil
}

func (ar *applicationsRouter) delete(ctx context.Context, w http.ResponseWriter, r *http.Request, vars map[string]string) error {
    user, err := ar.currentUser(vars)
    if err != nil {
        return err
    }

    err = ar.NewUserBroker(user).RemoveApplication(vars["name"])
    if err != nil {
        return err
    }

    w.WriteHeader(http.StatusNoContent)
    return nil
}

func (ar *applicationsRouter) start(ctx context.Context, w http.ResponseWriter, r *http.Request, vars map[string]string) error {
    user, err := ar.currentUser(vars)
    if err != nil {
        return err
    }
    return ar.NewUserBroker(user).StartApplication(vars["name"])
}

func (ar *applicationsRouter) stop(ctx context.Context, w http.ResponseWriter, r *http.Request, vars map[string]string) error {
    user, err := ar.currentUser(vars)
    if err != nil {
        return err
    }
    return ar.NewUserBroker(user).StopApplication(vars["name"])
}

func (ar *applicationsRouter) restart(ctx context.Context, w http.ResponseWriter, r *http.Request, vars map[string]string) error {
    user, err := ar.currentUser(vars)
    if err != nil {
        return err
    }
    return ar.NewUserBroker(user).RestartApplication(vars["name"])
}

func (ar *applicationsRouter) deploy(ctx context.Context, w http.ResponseWriter, r *http.Request, vars map[string]string) error {
    user, err := ar.currentUser(vars)
    if err != nil {
        return err
    }

    name, branch := vars["name"], r.FormValue("branch")
    err = ar.SCM.Deploy(user.Namespace, name, branch)
    if err != nil {
        http.Error(w, err.Error(), http.StatusBadRequest)
    } else {
        w.WriteHeader(http.StatusNoContent)
    }
    return nil
}

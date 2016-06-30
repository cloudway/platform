package console

import (
    "fmt"
    "strings"
    "strconv"
    "regexp"
    "sort"
    "errors"
    "time"
    "net/http"

    "github.com/Sirupsen/logrus"
    "github.com/gorilla/mux"
    "gopkg.in/authboss.v0"

    "github.com/cloudway/platform/container/conf/defaults"
    "github.com/cloudway/platform/container"
    "github.com/cloudway/platform/pkg/manifest"
    "github.com/cloudway/platform/auth/userdb"
    "github.com/cloudway/platform/container/conf"
)

func (con *Console) initApplicationsRoutes(gets *mux.Router, posts *mux.Router) {
    gets.HandleFunc("/applications", con.getApplications)
    gets.HandleFunc("/forms/applications", con.createApplicationForm)
    posts.HandleFunc("/applications", con.createApplication)
    gets.HandleFunc("/applications/{name}", con.getApplication)
    posts.HandleFunc("/applications/{name}/reload", con.reloadApplication)
    posts.HandleFunc("/applications/{name}/delete", con.removeApplication)
    posts.HandleFunc("/applications/{name}/scale", con.scaleApplication)
    posts.HandleFunc("/applications/{name}/services", con.createServices)
    posts.HandleFunc("/applications/{name}/services/{service}/delete", con.removeService)
}

type appListData struct {
    Name      string
    URL       string
    CreatedAt time.Time
    Framework string
    Plugins   []string
}

type appList []*appListData

func (a appList) Len() int { return len(a) }
func (a appList) Swap(i, j int) { a[i], a[j] = a[j], a[i] }
func (a appList) Less(i, j int) bool { return a[i].CreatedAt.After(a[j].CreatedAt) }

func (con *Console) getApplications(w http.ResponseWriter, r *http.Request) {
    user := con.currentUser(w, r)
    if user == nil {
        return
    }

    var apps []*appListData
    for name, a := range user.Applications {
        framework := ""
        plugins := make([]string, 0, len(a.Plugins))
        for _, tag := range a.Plugins {
            meta, err := con.Hub.GetPluginInfo(tag)
            if err != nil {
                plugins = append(plugins, tag)
            } else if meta.IsFramework() {
                framework = meta.DisplayName
            } else {
                plugins = append(plugins, meta.DisplayName)
            }
        }

        apps = append(apps, &appListData{
            Name:       name,
            URL:        fmt.Sprintf("http://%s-%s.%s", name, user.Namespace, defaults.Domain()),
            CreatedAt:  a.CreatedAt,
            Framework:  framework,
            Plugins:    plugins,
        })
    }
    sort.Sort(appList(apps))

    data := con.layoutUserData(w, r, user)
    data.MergeKV("apps", apps)
    con.mustRender(w, r, "applications", data)
}

func (con *Console) createApplicationForm(w http.ResponseWriter, r *http.Request) {
    user := con.currentUser(w, r)
    if user == nil {
        return
    }

    data := con.layoutUserData(w, r, user)
    data.MergeKV("domain", defaults.Domain())
    data.MergeKV("available_plugins", con.Hub.ListPlugins("", ""))
    con.mustRender(w, r, "createapp", data)
}

func (con *Console) createApplication(w http.ResponseWriter, r *http.Request) {
    user := con.currentUser(w, r)
    if user == nil {
        return
    }

    var cs []*container.Container
    opts, tags, err := parseCreateOptions(r)
    if err == nil {
        cs, err = con.NewUserBroker(user).CreateApplication(opts, tags)
    }

    if err != nil {
        data := con.layoutUserData(w, r, user)
        data.MergeKV("error", err)
        data.MergeKV("name", r.PostForm.Get("name"))
        data.MergeKV("plugins", r.PostForm.Get("plugins"))
        data.MergeKV("repo", r.PostForm.Get("repo"))
        data.MergeKV("domain", defaults.Domain())
        data.MergeKV("available_plugins", con.Hub.ListPlugins("", ""))
        con.mustRender(w, r, "createapp", data)
        return
    }

    err = startContainers(cs, (*container.Container).Start)
    if err != nil {
        logrus.Error(err)
        con.error(w, r, http.StatusInternalServerError, err.Error(), "/applications")
        return
    }

    http.Redirect(w, r, "/applications/"+opts.Name, http.StatusFound)
}

var namePattern = regexp.MustCompile("^[a-z][a-z_0-9]*$")

func parseCreateOptions(r *http.Request) (opts container.CreateOptions, tags []string, err error) {
    err = r.ParseForm()
    if err != nil {
        return
    }

    opts = container.CreateOptions{
        Name:    r.PostForm.Get("name"),
        Repo:    r.PostForm.Get("repo"),
        Scaling: 1,
    }

    if !namePattern.MatchString(opts.Name) {
        err = errors.New("应用名称只能包含小写英文字母、数字、或者下划线")
        return
    }

    tags = strings.Fields(r.PostForm.Get("plugins"))
    if len(tags) == 0 {
        err = errors.New("应用框架不能为空")
        return
    }

    return
}

func (con *Console) createServices(w http.ResponseWriter, r *http.Request) {
    user := con.currentUser(w, r)
    if user == nil {
        return
    }

    var cs []*container.Container
    opts, tags, err := con.parseServiceCreateOptions(r)
    if err == nil {
        cs, err = con.NewUserBroker(user).CreateServices(opts, tags)
    }

    if err != nil {
        data := con.layoutUserData(w, r, user)
        data.MergeKV("error", err)
        con.showApplication(w, r, user, data)
        return
    }

    err = startContainers(cs, (*container.Container).Start)
    if err != nil {
        logrus.Error(err)
        con.error(w, r, http.StatusInternalServerError, err.Error(), "/applications")
        return
    }

    http.Redirect(w, r, "/applications/"+opts.Name, http.StatusFound)
}

func (con *Console) parseServiceCreateOptions(r *http.Request) (opts container.CreateOptions, tags []string, err error) {
    err = r.ParseForm()
    if err != nil {
        return
    }

    opts = container.CreateOptions{Name: mux.Vars(r)["name"]}
    tags = strings.Fields(r.PostForm.Get("plugins"))

    if len(tags) == 0 {
        err = errors.New("服务插件不能为空")
        return
    }

    return
}

func startContainers(containers []*container.Container, fn func(*container.Container) error) error {
    err := container.ResolveServiceDependencies(containers)
    if err != nil {
        return err
    }

    errChan := make(chan error, 1)
    go func() {
        for _, c := range containers {
            if err := fn(c); err != nil {
                errChan <- err
                return
            }
        }
        errChan <- nil
    }()

    timer := time.NewTimer(time.Second * 10)
    select {
    case err = <-errChan:
        timer.Stop()
        return err
    case <-timer.C:
        return nil
    }
}

type appData struct {
    Name        string
    URL         string
    CloneURL    string
    Services    []serviceData
}

type serviceData struct {
    ID          string
    Name        string
    DisplayName string
    PluginName  string
    Category    manifest.Category
    IP          string
    Ports       string
    State       string
}

func (con *Console) getApplication(w http.ResponseWriter, r *http.Request) {
    user := con.currentUser(w, r)
    if user == nil {
        return
    }

    data := con.layoutUserData(w, r, user)
    con.showApplication(w, r, user, data)
}

func (con *Console) showApplication(w http.ResponseWriter, r *http.Request, user *userdb.BasicUser, data authboss.HTMLData) {
    name := mux.Vars(r)["name"]
    app := user.Applications[name]

    if app == nil {
        con.error(w, r, http.StatusNotFound, "应用未找到", "/applications")
        return
    }

    appData := &appData{Name: name}
    appData.URL = fmt.Sprintf("http://%s-%s.%s", name, user.Namespace, defaults.Domain())

    cloneURL := conf.Get("scm.clone_url")
    if cloneURL != "" {
        cloneURL = strings.Replace(cloneURL, "<namespace>", user.Namespace, -1)
        cloneURL = strings.Replace(cloneURL, "<repo>", name, -1)
        appData.CloneURL = cloneURL
    }

    cs, err := con.FindAll(name, user.Namespace)
    if err != nil {
        logrus.Error(err)
        http.Error(w, "Internal server error", http.StatusInternalServerError)
        return
    }

    scale := 0
    services := make([]serviceData, len(cs))
    for i, c := range cs {
        meta, err := con.Hub.GetPluginInfo(c.PluginTag())
        if err == nil {
            services[i].PluginName = meta.Name
            services[i].DisplayName = meta.DisplayName
            services[i].Ports = getPrivatePorts(meta)
        } else {
            tag := c.PluginTag()
            services[i].PluginName = strings.SplitN(tag, ":", 2)[0]
            services[i].DisplayName = tag
        }

        services[i].ID       = c.ID
        services[i].Name     = c.ServiceName()
        services[i].Category = c.Category()
        services[i].IP       = c.IP()
        services[i].State    = c.ActiveState().String()

        if c.Category().IsFramework() {
            scale++
        }
    }

    var plugins []*manifest.Plugin
    for _, meta := range con.Hub.ListPlugins("", manifest.Service) {
        var remove bool
        for _, s := range services {
            if meta.Name == s.PluginName {
                remove = true
                break
            }
        }
        if !remove {
            plugins = append(plugins, meta)
        }
    }

    appData.Services = services
    data.MergeKV("app", appData)
    data.MergeKV("scale", scale)
    data.MergeKV("available_plugins", plugins)
    con.mustRender(w, r, "application", data)
}

func getPrivatePorts(meta *manifest.Plugin) string {
    var ports []string
    for _, ep := range meta.GetEndpoints("", "") {
        port := strconv.FormatInt(int64(ep.PrivatePort), 10)
        exists := false
        for _, p := range ports {
            if p == port {
                exists = true
                break
            }
        }
        if !exists {
            ports = append(ports, port)
        }
    }
    return strings.Join(ports, ",")
}

func (con *Console) reloadApplication(w http.ResponseWriter, r *http.Request) {
    user := con.currentUser(w, r)
    if user == nil {
        return
    }

    name := mux.Vars(r)["name"]
    cs, err := con.FindAll(name, user.Namespace)
    if err == nil {
        err = startContainers(cs, (*container.Container).Restart)
    }

    if err != nil {
        logrus.Error(err)
        http.Error(w, err.Error(), http.StatusInternalServerError)
        return
    }

    http.Redirect(w, r, "/applications/"+name, http.StatusFound)
}

func (con *Console) removeApplication(w http.ResponseWriter, r *http.Request) {
    user := con.currentUser(w, r)
    if user == nil {
        return
    }

    name := mux.Vars(r)["name"]
    err := con.NewUserBroker(user).RemoveApplication(name)
    if con.badRequest(w, r, err, "/applications") {
        return
    } else {
        http.Redirect(w, r, "/applications", http.StatusFound)
    }
}

func (con *Console) removeService(w http.ResponseWriter, r *http.Request) {
    user := con.currentUser(w, r)
    if user == nil {
        return
    }

    name := mux.Vars(r)["name"]
    service := mux.Vars(r)["service"]
    err := con.NewUserBroker(user).RemoveService(name, service)
    if con.badRequest(w, r, err, "/applications/"+name) {
        return
    } else {
        http.Redirect(w, r, "/applications/"+name, http.StatusFound)
    }
}

func (con *Console) scaleApplication(w http.ResponseWriter, r *http.Request) {
    user := con.currentUser(w, r)
    if user == nil {
        return
    }

    name := mux.Vars(r)["name"]
    scale, err := strconv.Atoi(r.FormValue("scale"))
    if con.badRequest(w, r, err, "/applications/"+name) {
        return
    }

    cs, err := con.NewUserBroker(user).ScaleApplication(name, scale)
    if con.badRequest(w, r, err, "/applications/"+name) {
        return
    }

    err = startContainers(cs, (*container.Container).Start)
    if err != nil {
        logrus.Error(err)
        http.Error(w, err.Error(), http.StatusInternalServerError)
        return
    }

    http.Redirect(w, r, "/applications/"+name, http.StatusFound)
}

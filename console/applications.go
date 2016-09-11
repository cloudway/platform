package console

import (
	"crypto/md5"
	"crypto/rand"
	"encoding/hex"
	"encoding/json"
	"errors"
	"fmt"
	"net/http"
	"regexp"
	"sort"
	"strconv"
	"strings"
	"sync"
	"time"

	"github.com/Sirupsen/logrus"
	"github.com/gorilla/mux"
	"golang.org/x/net/context"
	"gopkg.in/authboss.v0"

	"github.com/cloudway/platform/auth/userdb"
	"github.com/cloudway/platform/config"
	"github.com/cloudway/platform/config/defaults"
	"github.com/cloudway/platform/container"
	"github.com/cloudway/platform/pkg/manifest"
	"github.com/cloudway/platform/scm"
)

func (con *Console) initApplicationsRoutes(gets *mux.Router, posts *mux.Router) {
	gets.HandleFunc("/applications", con.getApplications)
	gets.HandleFunc("/forms/applications", con.createApplicationForm)
	posts.HandleFunc("/applications", con.createApplication)
	gets.HandleFunc("/applications/{name}", con.getApplication)
	gets.HandleFunc("/applications/{name}/status", con.getApplicationStatus)
	gets.HandleFunc("/applications/{name}/settings", con.getApplicationSettings)
	posts.HandleFunc("/applications/{name}/host", con.addHost)
	posts.HandleFunc("/applications/{name}/host/delete", con.removeHost)
	posts.HandleFunc("/applications/{name}/reload", con.restartApplication)
	posts.HandleFunc("/applications/{name}/reload/async", con.asyncRestartApplication)
	posts.HandleFunc("/applications/{name}/deploy", con.deployApplication)
	posts.HandleFunc("/applications/{name}/scale", con.scaleApplication)
	posts.HandleFunc("/applications/{name}/delete", con.removeApplication)
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

func (a appList) Len() int           { return len(a) }
func (a appList) Swap(i, j int)      { a[i], a[j] = a[j], a[i] }
func (a appList) Less(i, j int) bool { return a[i].CreatedAt.After(a[j].CreatedAt) }

func (con *Console) appDNS(name, namespace string) string {
	return fmt.Sprintf("%s-%s.%s", name, namespace, defaults.Domain())
}

func (con *Console) appURL(name, namespace string) string {
	host, port := con.baseURL.Host, ""
	if i := strings.IndexRune(host, ':'); i != -1 {
		port = host[i:]
	}
	return fmt.Sprintf("%s://%s-%s.%s%s", con.baseURL.Scheme, name, namespace, defaults.Domain(), port)
}

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
			Name:      name,
			URL:       con.appURL(name, user.Namespace),
			CreatedAt: a.CreatedAt,
			Framework: framework,
			Plugins:   plugins,
		})
	}
	sort.Sort(appList(apps))

	data := con.layoutUserData(w, r, user)
	data.MergeKV("apps", apps)
	con.mustRender(w, r, "app_list", data)
}

func (con *Console) createApplicationForm(w http.ResponseWriter, r *http.Request) {
	user := con.currentUser(w, r)
	if user == nil {
		return
	}

	data := con.layoutUserData(w, r, user)
	data.MergeKV("domain", defaults.Domain())
	data.MergeKV("available_plugins", con.NewUserBroker(user).GetInstalledPlugins(""))
	con.mustRender(w, r, "app_create", data)
}

func (con *Console) createApplication(w http.ResponseWriter, r *http.Request) {
	user := con.currentUser(w, r)
	if user == nil {
		return
	}

	var cs []*container.Container
	opts, tags, err := parseCreateOptions(r)
	if err == nil {
		_, cs, err = con.NewUserBroker(user).CreateApplication(opts, tags)
	}

	if err != nil {
		data := con.layoutUserData(w, r, user)
		data.MergeKV("error", err)
		data.MergeKV("name", r.PostForm.Get("name"))
		data.MergeKV("framework", r.PostForm.Get("framework"))
		data.MergeKV("services", r.PostForm.Get("services"))
		data.MergeKV("repo", r.PostForm.Get("repo"))
		data.MergeKV("domain", defaults.Domain())
		data.MergeKV("available_plugins", con.NewUserBroker(user).GetInstalledPlugins(""))
		con.mustRender(w, r, "app_create", data)
		return
	}

	err = con.startContainers(cs)
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

	framework := r.PostForm.Get("framework")
	services := strings.Fields(r.PostForm.Get("services"))
	if framework == "" {
		err = errors.New("应用框架不能为空")
		return
	}
	tags = append([]string{framework}, services...)
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

	err = con.startContainers(cs)
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
	tags = strings.Fields(r.PostForm.Get("services"))

	if len(tags) == 0 {
		err = errors.New("服务插件不能为空")
		return
	}

	return
}

func (con *Console) startContainers(containers []*container.Container) error {
	errChan := make(chan error, 1)
	go func() {
		errChan <- con.Broker.StartContainers(context.Background(), containers)
	}()

	timer := time.NewTimer(time.Second * 10)
	select {
	case err := <-errChan:
		timer.Stop()
		return err
	case <-timer.C:
		return nil
	}
}

type appData struct {
	Name       string
	DNS        string
	URL        string
	CloneURL   string
	Branch     *scm.Branch
	Branches   []*scm.Branch
	Frameworks []serviceData
	Services   []serviceData
	Hosts      []string
	Scale      int
}

type serviceData struct {
	ID          string
	Name        string
	DisplayName string
	Logo        string
	PluginTag   string
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
	ctx := context.Background()

	if app == nil {
		con.error(w, r, http.StatusNotFound, "应用未找到", "/applications")
		return
	}

	appData := &appData{
		Name: name,
		DNS:  con.appDNS(name, user.Namespace),
		URL:  con.appURL(name, user.Namespace),
	}

	cloneURL := config.Get("scm.clone_url")
	if cloneURL != "" {
		cloneURL = strings.Replace(cloneURL, "<namespace>", user.Namespace, -1)
		cloneURL = strings.Replace(cloneURL, "<repo>", name, -1)
		appData.CloneURL = cloneURL
	}

	cs, err := con.FindAll(ctx, name, user.Namespace)
	if err != nil {
		logrus.Error(err)
		http.Error(w, "Internal server error", http.StatusInternalServerError)
		return
	}

	var (
		frameworks = make([]serviceData, 0, len(cs))
		services   = make([]serviceData, 0, len(cs))
		scale      = 0
	)
	for _, c := range cs {
		service := serviceData{
			ID:       c.ID,
			Name:     c.ServiceName(),
			Category: c.Category(),
			IP:       c.IP(),
			State:    c.ActiveState(ctx).String(),
		}

		tag := c.PluginTag()
		if meta, err := con.Hub.GetPluginInfo(tag); err == nil {
			service.PluginTag = meta.Tag
			service.PluginName = meta.Name
			service.DisplayName = meta.DisplayName
			service.Logo = meta.Logo
			service.Ports = getPrivatePorts(meta)
		} else {
			service.PluginTag = tag
			service.PluginName = strings.SplitN(tag, ":", 2)[0]
			service.DisplayName = tag
		}

		if c.Category().IsFramework() {
			scale++
			frameworks = append(frameworks, service)
		} else {
			services = append(services, service)
		}
	}

	var plugins []*manifest.Plugin
	for _, meta := range con.NewUserBroker(user).GetInstalledPlugins(manifest.Service) {
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
	appData.Frameworks = frameworks
	appData.Scale = scale

	data.MergeKV("app", appData)
	data.MergeKV("available_plugins", plugins)
	con.mustRender(w, r, "app", data)
}

func getPrivatePorts(meta *manifest.Plugin) string {
	var ports []string
	for _, ep := range meta.GetEndpoints("", "", "") {
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

func (con *Console) getApplicationSettings(w http.ResponseWriter, r *http.Request) {
	user := con.currentUser(w, r)
	if user == nil {
		return
	}

	data := con.layoutUserData(w, r, user)
	con.showApplicationSettings(w, r, user, data)
}

func (con *Console) addHost(w http.ResponseWriter, r *http.Request) {
	user := con.currentUser(w, r)
	if user == nil {
		return
	}

	name := mux.Vars(r)["name"]
	host := r.FormValue("hostname")
	err := con.NewUserBroker(user).AddHost(name, host)

	if err != nil {
		data := con.layoutUserData(w, r, user)
		data.MergeKV("error", err)
		con.showApplicationSettings(w, r, user, data)
		return
	}

	http.Redirect(w, r, "/applications/"+name+"/settings", http.StatusFound)
}

func (con *Console) removeHost(w http.ResponseWriter, r *http.Request) {
	user := con.currentUser(w, r)
	if user == nil {
		return
	}

	name := mux.Vars(r)["name"]
	host := r.FormValue("hostname")
	err := con.NewUserBroker(user).RemoveHost(name, host)
	if !con.badRequest(w, r, err, "/applications/"+name+"/settings") {
		http.Redirect(w, r, "/applications/"+name+"/settings", http.StatusFound)
	}
}

func (con *Console) showApplicationSettings(w http.ResponseWriter, r *http.Request, user *userdb.BasicUser, data authboss.HTMLData) {
	name := mux.Vars(r)["name"]
	app := user.Applications[name]

	if app == nil {
		con.error(w, r, http.StatusNotFound, "应用未找到", "/applications")
		return
	}

	appData := &appData{
		Name: name,
		DNS:  con.appDNS(name, user.Namespace),
		URL:  con.appURL(name, user.Namespace),
	}

	cloneURL := config.Get("scm.clone_url")
	if cloneURL != "" {
		cloneURL = strings.Replace(cloneURL, "<namespace>", user.Namespace, -1)
		cloneURL = strings.Replace(cloneURL, "<repo>", name, -1)
		appData.CloneURL = cloneURL
	}

	branch, err := con.SCM.GetDeploymentBranch(user.Namespace, name)
	if err != nil {
		logrus.Error(err)
	} else {
		appData.Branch = branch
	}

	branches, err := con.SCM.GetDeploymentBranches(user.Namespace, name)
	if err != nil {
		logrus.Error(err)
	} else {
		appData.Branches = branches
	}

	appData.Hosts = app.Hosts

	data.MergeKV("app", appData)
	con.mustRender(w, r, "app_settings", data)
}

func (con *Console) restartApplication(w http.ResponseWriter, r *http.Request) {
	name := mux.Vars(r)["name"]
	user := con.currentUser(w, r)
	if user == nil {
		return
	}

	err := con.NewUserBroker(user).RestartApplication(name)
	if err != nil {
		logrus.Error(err)
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}

	http.Redirect(w, r, "/applications/"+name, http.StatusFound)
}

var asyncTasks = make(map[string]bool)
var muTask sync.RWMutex

func (con *Console) asyncRestartApplication(w http.ResponseWriter, r *http.Request) {
	name := mux.Vars(r)["name"]
	user := con.currentUser(w, r)
	if user == nil {
		return
	}

	var b [16]byte
	rand.Read(b[:])
	buf := name + "-" + user.Namespace + ":" + string(b[:])
	hash := md5.Sum([]byte(buf))
	id := hex.EncodeToString(hash[:])

	w.Header().Set("Content-Type", "text/plain")
	w.WriteHeader(http.StatusOK)
	w.Write([]byte(id))

	muTask.Lock()
	asyncTasks[id] = true
	muTask.Unlock()

	go func() {
		defer func() {
			muTask.Lock()
			delete(asyncTasks, id)
			muTask.Unlock()
		}()

		err := con.NewUserBroker(user).RestartApplication(name)
		if err != nil {
			logrus.Error(err)
		}
	}()
}

type stateData struct {
	ID, State string
}

type statusData struct {
	Status string
	States []stateData
}

func (con *Console) getApplicationStatus(w http.ResponseWriter, r *http.Request) {
	user := con.currentUser(w, r)
	if user == nil {
		return
	}

	id := r.FormValue("id")
	name := mux.Vars(r)["name"]
	ctx := context.Background()

	var inprogress bool
	if id != "" {
		muTask.RLock()
		inprogress = asyncTasks[id]
		muTask.RUnlock()
	}

	cs, err := con.FindAll(ctx, name, user.Namespace)
	if err != nil {
		w.WriteHeader(http.StatusInternalServerError)
		return
	}

	status := statusData{}
	status.States = make([]stateData, len(cs))
	for i, c := range cs {
		status.States[i].ID = c.ID
		status.States[i].State = c.ActiveState(ctx).String()
	}

	if inprogress {
		status.Status = "inprogress"
	} else {
		status.Status = "finished"
	}

	w.Header().Set("Content-Type", "application/json")
	w.WriteHeader(http.StatusOK)
	json.NewEncoder(w).Encode(&status)
}

func (con *Console) deployApplication(w http.ResponseWriter, r *http.Request) {
	user := con.currentUser(w, r)
	if user == nil {
		return
	}

	name := mux.Vars(r)["name"]
	branch := r.FormValue("branch")
	err := con.SCM.Deploy(user.Namespace, name, branch, nil, nil)
	if err != nil {
		http.Error(w, err.Error(), http.StatusBadRequest)
	} else {
		w.WriteHeader(http.StatusNoContent)
	}
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

	err = con.startContainers(cs)
	if err != nil {
		logrus.Error(err)
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}

	http.Redirect(w, r, "/applications/"+name, http.StatusFound)
}

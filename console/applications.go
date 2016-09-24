package console

import (
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"net/http"
	"regexp"
	"sort"
	"strconv"
	"strings"
	"time"
	"unicode/utf8"

	"github.com/Sirupsen/logrus"
	"github.com/docker/engine-api/types"
	"github.com/gorilla/mux"
	"golang.org/x/net/websocket"
	"gopkg.in/authboss.v0"

	"github.com/cloudway/platform/auth/userdb"
	"github.com/cloudway/platform/broker"
	"github.com/cloudway/platform/config"
	"github.com/cloudway/platform/config/defaults"
	"github.com/cloudway/platform/container"
	"github.com/cloudway/platform/pkg/manifest"
	"github.com/cloudway/platform/pkg/serverlog"
	"github.com/cloudway/platform/scm"
)

func (con *Console) initApplicationsRoutes(gets *mux.Router, posts *mux.Router) {
	gets.HandleFunc("/applications", con.getApplications)
	gets.HandleFunc("/applications/create/form", con.createApplicationForm)
	gets.HandleFunc("/applications/create/ws", con.createApplication)
	gets.HandleFunc("/applications/{name}", con.getApplication)
	gets.HandleFunc("/applications/{name}/settings", con.getApplicationSettings)
	posts.HandleFunc("/applications/{name}/host", con.addHost)
	posts.HandleFunc("/applications/{name}/host/delete", con.removeHost)
	posts.HandleFunc("/applications/{name}/reload", con.restartApplication)
	gets.HandleFunc("/applications/{name}/reload/ws", con.wsRestartApplication)
	gets.HandleFunc("/applications/{name}/deploy", con.deployApplication)
	posts.HandleFunc("/applications/{name}/scale", con.scaleApplication)
	posts.HandleFunc("/applications/{name}/delete", con.removeApplication)
	posts.HandleFunc("/applications/{name}/services", con.createServices)
	posts.HandleFunc("/applications/{name}/services/{service}/delete", con.removeService)

	gets.HandleFunc("/shell/{id}/open", con.shellOpen)
	gets.HandleFunc("/shell/{id}/session", con.shellSession)
	posts.HandleFunc("/shell/{id}/resize", con.shellResize)
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

func (con *Console) wsURL() string {
	scheme := "ws"
	if con.baseURL.Scheme == "https" {
		scheme = "wss"
	}
	return scheme + "://" + con.baseURL.Host
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
	data.MergeKV("plugins", con.NewUserBroker(user).GetInstalledPlugins(""))
	data.MergeKV("ws", con.wsURL()+"/applications/create/ws")
	con.mustRender(w, r, "app_create", data)
}

type jsonWriter struct {
	enc *json.Encoder
}

func (w jsonWriter) Write(p []byte) (n int, err error) {
	data := map[string]string{"msg": string(p)}
	err = w.enc.Encode(data)
	return len(p), err
}

func (con *Console) createApplication(w http.ResponseWriter, r *http.Request) {
	user := con.currentUser(w, r)
	if user == nil {
		return
	}

	h := func(conn *websocket.Conn) {
		opts, tags, err := parseCreateOptions(r)
		if err != nil {
			data := map[string]string{"err": err.Error()}
			json.NewEncoder(conn).Encode(data)
			return
		}

		jw := jsonWriter{enc: json.NewEncoder(conn)}
		opts.Log = serverlog.Encap(jw, jw)

		br := con.NewUserBroker(user)
		_, cs, err := br.CreateApplication(opts, tags)
		if err == nil {
			err = br.StartContainers(cs, opts.Log)
		}
		if err != nil {
			data := map[string]string{"err": err.Error()}
			json.NewEncoder(conn).Encode(data)
		}
	}

	srv := websocket.Server{Handler: h}
	srv.ServeHTTP(w, r)
}

var namePattern = regexp.MustCompile("^[a-z][a-z_0-9]*$")

func parseCreateOptions(r *http.Request) (opts container.CreateOptions, tags []string, err error) {
	err = r.ParseForm()
	if err != nil {
		return
	}

	opts = container.CreateOptions{
		Name:    r.Form.Get("name"),
		Repo:    r.Form.Get("repo"),
		Scaling: 1,
	}

	if !namePattern.MatchString(opts.Name) {
		err = errors.New("应用名称只能包含小写英文字母、数字、或者下划线")
		return
	}

	framework := r.Form.Get("framework")
	services := strings.Fields(r.Form.Get("services"))
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
	br := con.NewUserBroker(user)

	var cs []*container.Container
	opts, tags, err := con.parseServiceCreateOptions(r)
	if err == nil {
		cs, err = br.CreateServices(opts, tags)
	}

	if err != nil {
		data := con.layoutUserData(w, r, user)
		data.MergeKV("error", err)
		con.showApplication(w, r, user, data)
		return
	}

	err = startContainers(br, cs)
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

func startContainers(br *broker.UserBroker, containers []*container.Container) error {
	errChan := make(chan error, 1)
	go func() {
		errChan <- br.StartContainers(containers, nil)
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
	WS         string
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
		WS:   con.wsURL(),
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
		WS:   con.wsURL(),
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

	err := con.NewUserBroker(user).RestartApplication(name, nil)
	if err != nil {
		logrus.Error(err)
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}

	http.Redirect(w, r, "/applications/"+name, http.StatusFound)
}

func (con *Console) wsRestartApplication(w http.ResponseWriter, r *http.Request) {
	name := mux.Vars(r)["name"]
	user := con.currentUser(w, r)
	if user == nil {
		return
	}

	type state struct {
		ID, State string
	}

	h := func(conn *websocket.Conn) {
		done := make(chan error)
		go func() {
			done <- con.NewUserBroker(user).RestartApplication(name, nil)
		}()

		var (
			ctx   = context.Background()
			enc   = json.NewEncoder(conn)
			tick  = time.NewTicker(500 * time.Millisecond)
			count = -1
		)
		for {
			select {
			case <-done:
				count = 20
			case <-tick.C:
			}

			cs, err := con.FindAll(ctx, name, user.Namespace)
			if err == nil {
				started := true
				states := make([]state, len(cs))
				for i, c := range cs {
					state := c.ActiveState(ctx)
					states[i].ID = c.ID
					states[i].State = state.String()
					if !(state == manifest.StateRunning || state == manifest.StateFailed) {
						started = false
					}
				}
				enc.Encode(states)
				if started {
					tick.Stop()
					return
				}
			}

			if count > 0 {
				count--
				if count == 0 {
					tick.Stop()
					return
				}
			}
		}
	}

	srv := websocket.Server{Handler: h, Handshake: nil}
	srv.ServeHTTP(w, r)
}

func (con *Console) deployApplication(w http.ResponseWriter, r *http.Request) {
	user := con.currentUser(w, r)
	if user == nil {
		return
	}

	name := mux.Vars(r)["name"]
	branch := r.FormValue("branch")

	h := func(conn *websocket.Conn) {
		jw := jsonWriter{enc: json.NewEncoder(conn)}
		log := serverlog.Encap(jw, jw)
		err := con.SCM.Deploy(user.Namespace, name, branch, log)
		if err != nil {
			data := map[string]string{"err": err.Error()}
			json.NewEncoder(conn).Encode(data)
		}
	}

	srv := websocket.Server{Handler: h}
	srv.ServeHTTP(w, r)
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

	br := con.NewUserBroker(user)
	cs, err := br.ScaleApplication(name, scale)
	if con.badRequest(w, r, err, "/applications/"+name) {
		return
	}

	err = startContainers(br, cs)
	if err != nil {
		logrus.Error(err)
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}

	http.Redirect(w, r, "/applications/"+name, http.StatusFound)
}

func (con *Console) shellOpen(w http.ResponseWriter, r *http.Request) {
	user := con.currentUser(w, r)
	if user == nil {
		return
	}

	id := mux.Vars(r)["id"]
	container, err := con.Inspect(context.Background(), id)
	if err != nil || container.Namespace != user.Namespace {
		http.Redirect(w, r, "/applications", http.StatusNotFound)
		return
	}

	data := con.layoutUserData(w, r, user)
	data.MergeKV("ws", con.wsURL()+"/shell/"+id+"/session")
	data.MergeKV("id", id)
	data.MergeKV("name", container.Name)
	data.MergeKV("service", container.ServiceName())
	con.mustRender(w, r, "shell", data)
}

func (con *Console) shellSession(w http.ResponseWriter, r *http.Request) {
	user := con.currentUser(w, r)
	if user == nil {
		return
	}

	id := mux.Vars(r)["id"]
	container, err := con.Inspect(context.Background(), id)
	if err != nil || container.Namespace != user.Namespace {
		http.Redirect(w, r, "/applications", http.StatusNotFound)
		return
	}

	h := func(conn *websocket.Conn) {
		ctx := context.Background()
		cmd := []string{"/usr/bin/cwctl", "sh", "-e", "TERM=xterm-256color", "cwsh"}
		execConfig := types.ExecConfig{
			Tty:          true,
			AttachStdin:  true,
			AttachStdout: true,
			AttachStderr: true,
			Cmd:          cmd,
		}

		execResp, err := container.ContainerExecCreate(ctx, container.ID, execConfig)
		if err != nil {
			return
		}
		execId := execResp.ID

		resp, err := container.ContainerExecAttach(ctx, execId, execConfig)
		if err != nil {
			return
		}
		defer resp.Close()

		// receive terminal size and send exec id
		var resize types.ResizeOptions
		if json.NewDecoder(conn).Decode(&resize) != nil {
			return
		}
		if json.NewEncoder(conn).Encode(&execResp) != nil {
			return
		}
		container.ContainerExecResize(ctx, execId, resize)

		// Pipe session to container and vice-versa
		go func() {
			io.Copy(resp.Conn, conn)
			resp.CloseWrite()
		}()

		var buf [4096]byte
		for {
			nr, er := resp.Reader.Read(buf[:])
			if nr > 0 {
				if utf8.Valid(buf[:nr]) {
					if _, ew := conn.Write(buf[:nr]); ew != nil {
						break
					}
				} else {
					if ew := writeUTF8(conn, buf[:nr]); ew != nil {
						break
					}
				}
			}
			if er != nil {
				break
			}
		}
	}

	srv := websocket.Server{Handler: h}
	srv.ServeHTTP(w, r)
}

func writeUTF8(w io.Writer, buf []byte) (err error) {
	var first, next int
	var max = len(buf)
	var utf [8]byte

	for next < max {
		r, sz := utf8.DecodeRune(buf[next:max])
		if r != utf8.RuneError {
			next += sz
		} else {
			if next > first {
				if _, err = w.Write(buf[first:next]); err != nil {
					return
				}
			}
			sz := utf8.EncodeRune(utf[:], rune(buf[next]))
			logrus.Debugf("encoded invalid utf-8 character %v to %s", buf[next], string(utf[0:sz]))
			if _, err = w.Write(utf[0:sz]); err != nil {
				return
			}
			next++
			first = next
		}
	}
	if next > first {
		_, err = w.Write(buf[first:next])
	}
	return
}

func (con *Console) shellResize(w http.ResponseWriter, r *http.Request) {
	err := r.ParseForm()
	if err != nil {
		return
	}

	id := mux.Vars(r)["id"]
	cols, _ := strconv.Atoi(r.PostForm.Get("cols"))
	rows, _ := strconv.Atoi(r.PostForm.Get("rows"))
	if cols > 0 && rows > 0 {
		resize := types.ResizeOptions{Width: cols, Height: rows}
		con.ContainerExecResize(context.Background(), id, resize)
	}
}

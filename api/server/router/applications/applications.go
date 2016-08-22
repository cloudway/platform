package applications

import (
	"compress/gzip"
	"encoding/json"
	"fmt"
	"io"
	"net/http"
	"net/url"
	"regexp"
	"strconv"
	"strings"

	"github.com/cloudway/platform/api/server/httputils"
	"github.com/cloudway/platform/api/server/middleware"
	"github.com/cloudway/platform/api/server/router"
	"github.com/cloudway/platform/api/types"
	"github.com/cloudway/platform/auth/userdb"
	"github.com/cloudway/platform/broker"
	"github.com/cloudway/platform/config"
	"github.com/cloudway/platform/config/defaults"
	"github.com/cloudway/platform/container"
	"github.com/cloudway/platform/scm"
	"golang.org/x/net/context"
)

type applicationsRouter struct {
	*broker.Broker
	routes []router.Route
}

func NewRouter(broker *broker.Broker) router.Router {
	r := &applicationsRouter{Broker: broker}

	r.routes = []router.Route{
		router.NewGetRoute("/applications/", r.list),
		router.NewGetRoute("/applications/{name:.*}/info", r.info),
		router.NewPostRoute("/applications/", r.create),
		router.NewDeleteRoute("/applications/{name:.*}", r.delete),
		router.NewPostRoute("/applications/{name:.*}/start", r.start),
		router.NewPostRoute("/applications/{name:.*}/stop", r.stop),
		router.NewPostRoute("/applications/{name:.*}/restart", r.restart),
		router.NewPostRoute("/applications/{name:.*}/deploy", r.deploy),
		router.NewGetRoute("/applications/{name:.*}/deploy", r.getDeployments),
		router.NewGetRoute("/applications/{name:.*}/repo", r.download),
		router.NewPutRoute("/applications/{name:.*}/repo", r.upload),
		router.NewGetRoute("/applications/{name:.*}/data", r.dump),
		router.NewPutRoute("/applications/{name:.*}/data", r.restore),
		router.NewPostRoute("/applications/{name:.*}/scale", r.scale),
		router.NewGetRoute("/applications/{name:.*}/services/{service:.*}/env/", r.environ),
		router.NewPostRoute("/applications/{name:.*}/services/{service:.*}/env/", r.setenv),
		router.NewGetRoute("/applications/{name:.*}/services/{service:.*}/env/{key:.*}", r.getenv),
	}

	return r
}

func (ar *applicationsRouter) Routes() []router.Route {
	return ar.routes
}

func (ar *applicationsRouter) currentUser(ctx context.Context) *userdb.BasicUser {
	return ctx.Value(middleware.UserKey).(*userdb.BasicUser)
}

func (ar *applicationsRouter) list(ctx context.Context, w http.ResponseWriter, r *http.Request, vars map[string]string) error {
	user := ar.currentUser(ctx)

	apps, err := ar.NewUserBroker(user).GetApplications()
	if err != nil {
		return err
	}

	var names []string
	for name := range apps {
		names = append(names, name)
	}
	return httputils.WriteJSON(w, http.StatusOK, names)
}

func (ar *applicationsRouter) info(ctx context.Context, w http.ResponseWriter, r *http.Request, vars map[string]string) error {
	var (
		user = ar.currentUser(ctx)
		name = vars["name"]
	)

	apps, err := ar.NewUserBroker(user).GetApplications()
	if err != nil {
		return err
	}

	app := apps[name]
	if app == nil {
		return httputils.NewStatusError(http.StatusNotFound)
	}

	info := types.ApplicationInfo{
		Name:      name,
		Namespace: user.Namespace,
		CreatedAt: app.CreatedAt,
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
	info.SSHURL = fmt.Sprintf("ssh://%s-%s@%s%s", name, user.Namespace, host, ":2200") // FIXME

	info.SCMType = ar.SCM.Type()
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

	cs, _ := ar.FindApplications(name, user.Namespace)
	info.Scaling = len(cs)

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

	user := ar.currentUser(ctx)
	br := ar.NewUserBroker(user)

	var req types.CreateApplication
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

	cs, err := br.CreateApplication(opts, tags)
	if err != nil {
		return err
	}

	if err = br.StartContainers(cs); err != nil {
		return err
	}

	vars["name"] = req.Name
	return ar.info(ctx, w, r, vars)
}

func (ar *applicationsRouter) delete(ctx context.Context, w http.ResponseWriter, r *http.Request, vars map[string]string) error {
	user := ar.currentUser(ctx)
	br := ar.NewUserBroker(user)

	if err := br.RemoveApplication(vars["name"]); err != nil {
		return err
	} else {
		w.WriteHeader(http.StatusNoContent)
		return nil
	}
}

func (ar *applicationsRouter) start(ctx context.Context, w http.ResponseWriter, r *http.Request, vars map[string]string) error {
	user := ar.currentUser(ctx)
	return ar.NewUserBroker(user).StartApplication(vars["name"])
}

func (ar *applicationsRouter) stop(ctx context.Context, w http.ResponseWriter, r *http.Request, vars map[string]string) error {
	user := ar.currentUser(ctx)
	return ar.NewUserBroker(user).StopApplication(vars["name"])
}

func (ar *applicationsRouter) restart(ctx context.Context, w http.ResponseWriter, r *http.Request, vars map[string]string) error {
	user := ar.currentUser(ctx)
	return ar.NewUserBroker(user).RestartApplication(vars["name"])
}

func (ar *applicationsRouter) deploy(ctx context.Context, w http.ResponseWriter, r *http.Request, vars map[string]string) error {
	user := ar.currentUser(ctx)
	name, branch := vars["name"], r.FormValue("branch")

	err := ar.SCM.Deploy(user.Namespace, name, branch)
	if err != nil {
		http.Error(w, err.Error(), http.StatusBadRequest)
	} else {
		w.WriteHeader(http.StatusNoContent)
	}
	return nil
}

func (ar *applicationsRouter) getDeployments(ctx context.Context, w http.ResponseWriter, r *http.Request, vars map[string]string) error {
	user := ar.currentUser(ctx)
	name := vars["name"]

	current, err := ar.SCM.GetDeploymentBranch(user.Namespace, name)
	if err != nil {
		return err
	}
	branches, err := ar.SCM.GetDeploymentBranches(user.Namespace, name)
	if err != nil {
		return err
	}

	resp := types.Deployments{
		Current:  convertBranchJson(current),
		Branches: convertBranchesJson(branches),
	}

	return httputils.WriteJSON(w, http.StatusOK, &resp)
}

func convertBranchJson(br *scm.Branch) *types.Branch {
	return &types.Branch{
		Id:        br.Id,
		DisplayId: br.DisplayId,
		Type:      br.Type,
	}
}

func convertBranchesJson(branches []scm.Branch) []types.Branch {
	result := make([]types.Branch, len(branches))
	for i := range branches {
		result[i] = *convertBranchJson(&branches[i])
	}
	return result
}

func (ar *applicationsRouter) download(ctx context.Context, w http.ResponseWriter, r *http.Request, vars map[string]string) error {
	user := ar.currentUser(ctx)

	tr, err := ar.NewUserBroker(user).Download(vars["name"])
	if err != nil {
		return err
	}
	defer tr.Close()

	w.Header().Set("Content-Type", "application/tar+gzip") // TODO: parse Accept header
	w.WriteHeader(http.StatusOK)

	zw := gzip.NewWriter(w)
	if _, err = io.Copy(zw, tr); err == nil {
		err = zw.Close()
	}
	return err
}

func (ar *applicationsRouter) upload(ctx context.Context, w http.ResponseWriter, r *http.Request, vars map[string]string) error {
	user := ar.currentUser(ctx)
	return ar.NewUserBroker(user).Upload(vars["name"], r.Body)
}

func (ar *applicationsRouter) dump(ctx context.Context, w http.ResponseWriter, r *http.Request, vars map[string]string) error {
	user := ar.currentUser(ctx)

	tr, err := ar.NewUserBroker(user).Dump(vars["name"])
	if err != nil {
		return err
	}
	defer tr.Close()

	w.Header().Set("Content-Type", "application/tar+gzip")
	w.WriteHeader(http.StatusOK)

	zw := gzip.NewWriter(w)
	if _, err = io.Copy(zw, tr); err == nil {
		err = zw.Close()
	}
	return err
}

func (ar *applicationsRouter) restore(ctx context.Context, w http.ResponseWriter, r *http.Request, vars map[string]string) error {
	user := ar.currentUser(ctx)
	return ar.NewUserBroker(user).Restore(vars["name"], r.Body)
}

func (ar *applicationsRouter) scale(ctx context.Context, w http.ResponseWriter, r *http.Request, vars map[string]string) error {
	user := ar.currentUser(ctx)
	name := vars["name"]
	scaling := r.FormValue("scale")

	var up, down bool
	if strings.HasPrefix(scaling, "+") {
		up = true
		scaling = scaling[1:]
	} else if strings.HasPrefix(scaling, "-") {
		down = true
		scaling = scaling[1:]
	}

	num, err := strconv.Atoi(scaling)
	if err != nil {
		http.Error(w, err.Error(), http.StatusBadRequest)
		return nil
	}

	if up || down {
		cs, err := ar.FindApplications(name, user.Namespace)
		if err != nil {
			return err
		}
		if up {
			num = len(cs) + num
		} else {
			num = len(cs) - num
		}
	}

	br := ar.NewUserBroker(user)
	cs, err := br.ScaleApplication(name, num)
	if err == nil {
		err = br.StartContainers(cs)
	}
	return err
}

func (ar *applicationsRouter) getContainers(namespace string, vars map[string]string) (cs []*container.Container, err error) {
	name, service := vars["name"], vars["service"]
	if service == "" || service == "*" || service == "_" {
		cs, err = ar.FindApplications(name, namespace)
	} else {
		cs, err = ar.FindService(name, namespace, service)
	}
	if err == nil && len(cs) == 0 {
		if service != "" {
			err = fmt.Errorf("Service '%s' not found in application '%s'", service, name)
		} else {
			err = broker.ApplicationNotFoundError(name)
		}
	}
	return cs, err
}

func (ar *applicationsRouter) getContainer(namespace string, vars map[string]string) (*container.Container, error) {
	cs, err := ar.getContainers(namespace, vars)
	if err == nil {
		return cs[0], nil
	} else {
		return nil, err
	}
}

func (ar *applicationsRouter) environ(ctx context.Context, w http.ResponseWriter, r *http.Request, vars map[string]string) error {
	user := ar.currentUser(ctx)

	container, err := ar.getContainer(user.Namespace, vars)
	if err != nil {
		return err
	}
	if info, err := container.GetInfo(); err != nil {
		return err
	} else {
		return httputils.WriteJSON(w, http.StatusOK, info.Env)
	}
}

func (ar *applicationsRouter) getenv(ctx context.Context, w http.ResponseWriter, r *http.Request, vars map[string]string) error {
	user := ar.currentUser(ctx)

	container, err := ar.getContainer(user.Namespace, vars)
	if err != nil {
		return err
	}
	if info, err := container.GetInfo(); err != nil {
		return err
	} else {
		key := vars["key"]
		val := info.Env[key]
		return httputils.WriteJSON(w, http.StatusOK, map[string]string{key: val})
	}
}

var validEnvKey = regexp.MustCompile(`^[a-zA-Z_0-9]+$`)

func (ar *applicationsRouter) setenv(ctx context.Context, w http.ResponseWriter, r *http.Request, vars map[string]string) error {
	if err := httputils.ParseForm(r); err != nil {
		return err
	}
	if err := httputils.CheckForJSON(r); err != nil {
		return err
	}

	_, rm := r.Form["remove"]
	var env map[string]string
	if err := json.NewDecoder(r.Body).Decode(&env); err != nil {
		return err
	}

	user := ar.currentUser(ctx)

	cs, err := ar.getContainers(user.Namespace, vars)
	if err != nil {
		return err
	}

	args := []string{"/usr/bin/cwctl", "setenv"}
	if rm {
		args = append(args, "-d")
		for k := range env {
			args = append(args, k)
		}
	} else {
		args = append(args, "--export")
		for k, v := range env {
			if !validEnvKey.MatchString(k) {
				http.Error(w, k+": Invalid environment variable key", http.StatusBadRequest)
				return nil
			}
			args = append(args, k+"="+v)
		}
	}

	for _, container := range cs {
		if err = container.ExecE("root", nil, nil, args...); err != nil {
			return err
		}
	}

	return nil
}

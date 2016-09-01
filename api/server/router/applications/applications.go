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
	"github.com/cloudway/platform/api/server/router"
	"github.com/cloudway/platform/api/types"
	"github.com/cloudway/platform/auth/userdb"
	"github.com/cloudway/platform/broker"
	"github.com/cloudway/platform/config"
	"github.com/cloudway/platform/config/defaults"
	"github.com/cloudway/platform/container"
	"github.com/cloudway/platform/pkg/serverlog"
	"github.com/cloudway/platform/scm"
	"golang.org/x/net/context"
)

const appPath = "/applications/{name:[^/]+}"
const servicePath = appPath + "/services/{service:[^/]+}"

type applicationsRouter struct {
	*broker.Broker
	routes []router.Route
}

func NewRouter(broker *broker.Broker) router.Router {
	r := &applicationsRouter{Broker: broker}

	r.routes = []router.Route{
		router.NewGetRoute("/applications/", r.list),
		router.NewPostRoute("/applications/", r.create),
		router.NewGetRoute(appPath, r.info),
		router.NewDeleteRoute(appPath, r.delete),
		router.NewPostRoute(appPath+"/start", r.start),
		router.NewPostRoute(appPath+"/stop", r.stop),
		router.NewPostRoute(appPath+"/restart", r.restart),
		router.NewPostRoute(appPath+"/deploy", r.deploy),
		router.NewGetRoute(appPath+"/deploy", r.getDeployments),
		router.NewGetRoute(appPath+"/repo", r.download),
		router.NewPutRoute(appPath+"/repo", r.upload),
		router.NewGetRoute(appPath+"/data", r.dump),
		router.NewPutRoute(appPath+"/data", r.restore),
		router.NewPostRoute(appPath+"/scale", r.scale),
		router.NewPostRoute(appPath+"/services/", r.createService),
		router.NewDeleteRoute(servicePath, r.removeService),
		router.NewGetRoute(servicePath+"/env/", r.environ),
		router.NewPostRoute(servicePath+"/env/", r.setenv),
		router.NewGetRoute(servicePath+"/env/{key:.*}", r.getenv),
	}

	return r
}

func (ar *applicationsRouter) Routes() []router.Route {
	return ar.routes
}

func (ar *applicationsRouter) list(ctx context.Context, w http.ResponseWriter, r *http.Request, vars map[string]string) error {
	user := httputils.UserFromContext(ctx)

	apps, err := ar.NewUserBroker(user, ctx).GetApplications()
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
		user = httputils.UserFromContext(ctx)
		br   = ar.NewUserBroker(user, ctx)
		name = vars["name"]
		app  *userdb.Application
	)

	apps, err := br.GetApplications()
	if err != nil {
		return err
	}

	if app = apps[name]; app == nil {
		return httputils.NewStatusError(http.StatusNotFound)
	}

	info, err := ar.getInfo(name, user.Namespace, app)
	if err != nil {
		return err
	}

	cs, _ := ar.FindApplications(ctx, name, user.Namespace)
	info.Scaling = len(cs)

	return httputils.WriteJSON(w, http.StatusOK, &info)
}

func (ar *applicationsRouter) getInfo(name, namespace string, app *userdb.Application) (info *types.ApplicationInfo, err error) {
	info = &types.ApplicationInfo{
		Name:      name,
		Namespace: namespace,
		CreatedAt: app.CreatedAt,
		Scaling:   1,
	}

	base, err := url.Parse(config.GetOrDefault("console.url", "http://api."+defaults.Domain()))
	if err != nil {
		return
	}

	host, port := base.Host, ""
	if i := strings.IndexRune(host, ':'); i != -1 {
		host, port = host[:i], host[i:]
	}
	info.URL = fmt.Sprintf("%s://%s-%s.%s%s", base.Scheme, name, namespace, defaults.Domain(), port)
	info.SSHURL = fmt.Sprintf("ssh://%s-%s@%s%s", name, namespace, host, ":2200") // FIXME

	info.SCMType = ar.SCM.Type()
	cloneURL := config.Get("scm.clone_url")
	if cloneURL != "" {
		cloneURL = strings.Replace(cloneURL, "<namespace>", namespace, -1)
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

	return
}

var namePattern = regexp.MustCompile("^[a-z][a-z_0-9]*$")

func (ar *applicationsRouter) create(ctx context.Context, w http.ResponseWriter, r *http.Request, vars map[string]string) error {
	if err := httputils.ParseForm(r); err != nil {
		return err
	}
	if err := httputils.CheckForJSON(r); err != nil {
		return err
	}

	user := httputils.UserFromContext(ctx)
	br := ar.NewUserBroker(user, ctx)

	var req types.CreateApplication
	if err := json.NewDecoder(r.Body).Decode(&req); err != nil {
		return err
	}

	opts := container.CreateOptions{
		Name:    req.Name,
		Repo:    req.Repo,
		Scaling: 1,
		Logger:  serverlog.NewLogWriter(w),
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

	app, cs, err := br.CreateApplication(opts, tags)
	if err != nil {
		serverlog.SendError(w, err)
		return nil
	}

	if err = br.StartContainers(ctx, cs); err != nil {
		serverlog.SendError(w, err)
		return nil
	}

	if info, err := ar.getInfo(req.Name, user.Namespace, app); err != nil {
		serverlog.SendError(w, err)
	} else {
		serverlog.SendObject(w, info)
	}

	return nil
}

func (ar *applicationsRouter) delete(ctx context.Context, w http.ResponseWriter, r *http.Request, vars map[string]string) error {
	user := httputils.UserFromContext(ctx)
	br := ar.NewUserBroker(user, ctx)

	if err := br.RemoveApplication(vars["name"]); err != nil {
		return err
	} else {
		w.WriteHeader(http.StatusNoContent)
		return nil
	}
}

func (ar *applicationsRouter) createService(ctx context.Context, w http.ResponseWriter, r *http.Request, vars map[string]string) error {
	if err := httputils.CheckForJSON(r); err != nil {
		return err
	}

	user := httputils.UserFromContext(ctx)
	br := ar.NewUserBroker(user, ctx)

	var tags []string
	if err := json.NewDecoder(r.Body).Decode(&tags); err != nil {
		return err
	}

	opts := container.CreateOptions{
		Name:   vars["name"],
		Logger: serverlog.NewLogWriter(w),
	}

	cs, err := br.CreateServices(opts, tags)
	if err != nil {
		serverlog.SendError(w, err)
		return nil
	}

	if err := br.StartContainers(ctx, cs); err != nil {
		serverlog.SendError(w, err)
		return nil
	}

	return nil
}

func (ar *applicationsRouter) removeService(ctx context.Context, w http.ResponseWriter, r *http.Request, vars map[string]string) error {
	user := httputils.UserFromContext(ctx)
	br := ar.NewUserBroker(user, ctx)

	name, service := vars["name"], vars["service"]
	if err := br.RemoveService(name, service); err != nil {
		return err
	} else {
		w.WriteHeader(http.StatusNoContent)
		return nil
	}
}

func (ar *applicationsRouter) start(ctx context.Context, w http.ResponseWriter, r *http.Request, vars map[string]string) error {
	user := httputils.UserFromContext(ctx)
	return ar.NewUserBroker(user, ctx).StartApplication(vars["name"])
}

func (ar *applicationsRouter) stop(ctx context.Context, w http.ResponseWriter, r *http.Request, vars map[string]string) error {
	user := httputils.UserFromContext(ctx)
	return ar.NewUserBroker(user, ctx).StopApplication(vars["name"])
}

func (ar *applicationsRouter) restart(ctx context.Context, w http.ResponseWriter, r *http.Request, vars map[string]string) error {
	user := httputils.UserFromContext(ctx)
	return ar.NewUserBroker(user, ctx).RestartApplication(vars["name"])
}

func (ar *applicationsRouter) deploy(ctx context.Context, w http.ResponseWriter, r *http.Request, vars map[string]string) error {
	user := httputils.UserFromContext(ctx)
	name, branch := vars["name"], r.FormValue("branch")

	logger := serverlog.NewLogWriter(w)
	err := ar.SCM.DeployWithLog(user.Namespace, name, branch, logger, logger)
	if err != nil {
		serverlog.SendError(w, err)
	}
	return nil
}

func (ar *applicationsRouter) getDeployments(ctx context.Context, w http.ResponseWriter, r *http.Request, vars map[string]string) error {
	user := httputils.UserFromContext(ctx)
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

func convertBranchesJson(branches []*scm.Branch) []*types.Branch {
	result := make([]*types.Branch, len(branches))
	for i := range branches {
		result[i] = convertBranchJson(branches[i])
	}
	return result
}

func (ar *applicationsRouter) download(ctx context.Context, w http.ResponseWriter, r *http.Request, vars map[string]string) error {
	user := httputils.UserFromContext(ctx)

	tr, err := ar.NewUserBroker(user, ctx).Download(vars["name"])
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
	if err := httputils.ParseForm(r); err != nil {
		return err
	}

	user := httputils.UserFromContext(ctx)
	_, binary := r.Form["binary"]
	logger := serverlog.NewLogWriter(w)

	err := ar.NewUserBroker(user, ctx).Upload(vars["name"], r.Body, binary, logger)
	if err != nil {
		serverlog.SendError(w, err)
	}
	return nil
}

func (ar *applicationsRouter) dump(ctx context.Context, w http.ResponseWriter, r *http.Request, vars map[string]string) error {
	user := httputils.UserFromContext(ctx)

	tr, err := ar.NewUserBroker(user, ctx).Dump(vars["name"])
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
	user := httputils.UserFromContext(ctx)
	return ar.NewUserBroker(user, ctx).Restore(vars["name"], r.Body)
}

func (ar *applicationsRouter) scale(ctx context.Context, w http.ResponseWriter, r *http.Request, vars map[string]string) error {
	user := httputils.UserFromContext(ctx)
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
		cs, err := ar.FindApplications(ctx, name, user.Namespace)
		if err != nil {
			return err
		}
		if up {
			num = len(cs) + num
		} else {
			num = len(cs) - num
		}
	}

	br := ar.NewUserBroker(user, ctx)
	cs, err := br.ScaleApplication(name, num)
	if err == nil {
		err = br.StartContainers(ctx, cs)
	}
	return err
}

func (ar *applicationsRouter) getContainers(ctx context.Context, namespace string, vars map[string]string) (cs []*container.Container, err error) {
	name, service := vars["name"], vars["service"]
	if service == "" || service == "*" || service == "_" {
		cs, err = ar.FindApplications(ctx, name, namespace)
	} else {
		cs, err = ar.FindService(ctx, name, namespace, service)
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

func (ar *applicationsRouter) getContainer(ctx context.Context, namespace string, vars map[string]string) (*container.Container, error) {
	cs, err := ar.getContainers(ctx, namespace, vars)
	if err == nil {
		return cs[0], nil
	} else {
		return nil, err
	}
}

func (ar *applicationsRouter) environ(ctx context.Context, w http.ResponseWriter, r *http.Request, vars map[string]string) error {
	user := httputils.UserFromContext(ctx)

	container, err := ar.getContainer(ctx, user.Namespace, vars)
	if err != nil {
		return err
	}
	if info, err := container.GetInfo(ctx); err != nil {
		return err
	} else {
		return httputils.WriteJSON(w, http.StatusOK, info.Env)
	}
}

func (ar *applicationsRouter) getenv(ctx context.Context, w http.ResponseWriter, r *http.Request, vars map[string]string) error {
	user := httputils.UserFromContext(ctx)

	container, err := ar.getContainer(ctx, user.Namespace, vars)
	if err != nil {
		return err
	}
	if info, err := container.GetInfo(ctx); err != nil {
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

	user := httputils.UserFromContext(ctx)

	cs, err := ar.getContainers(ctx, user.Namespace, vars)
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
		if err = container.ExecE(ctx, "root", nil, nil, args...); err != nil {
			return err
		}
	}

	return nil
}

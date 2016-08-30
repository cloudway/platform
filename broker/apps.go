package broker

import (
	"archive/tar"
	"compress/gzip"
	"crypto/rand"
	"crypto/sha1"
	"encoding/hex"
	errs "errors"
	"fmt"
	"io"
	"io/ioutil"
	"net/http"
	"os"
	"path/filepath"
	"strings"
	"sync"
	"time"

	"github.com/Sirupsen/logrus"
	"golang.org/x/net/context"

	"github.com/cloudway/platform/auth/userdb"
	"github.com/cloudway/platform/config/defaults"
	"github.com/cloudway/platform/container"
	"github.com/cloudway/platform/pkg/archive"
	"github.com/cloudway/platform/pkg/errors"
	"github.com/cloudway/platform/pkg/files"
	"github.com/cloudway/platform/pkg/manifest"
	"github.com/cloudway/platform/scm"
)

func (br *UserBroker) GetApplications() (apps map[string]*userdb.Application, err error) {
	if err = br.Refresh(); err != nil {
		return nil, err
	}
	return br.User.Basic().Applications, nil
}

func (br *UserBroker) CreateApplication(opts container.CreateOptions, tags []string) (containers []*container.Container, err error) {
	if err = br.Refresh(); err != nil {
		return nil, err
	}

	user := br.User.Basic()
	apps := user.Applications

	// check if the application already exists
	if apps[opts.Name] != nil {
		return nil, ApplicationExistError{opts.Name, user.Namespace}
	}

	if opts.Scaling == 0 {
		opts.Scaling = 1
	}

	// check plugins
	var (
		names     = make([]string, len(tags))
		plugins   = make([]*manifest.Plugin, len(tags))
		framework *manifest.Plugin
	)
	for i, tag := range tags {
		n, p, err := br.getPluginInfoWithNames(tag)
		if err != nil {
			return nil, err
		}
		if p.IsFramework() {
			if framework != nil {
				return nil, fmt.Errorf("Multiple framework plugins specified: %s and %s", p.Name, framework.Name)
			}
			framework = p
			n = ""
		} else if !p.IsService() {
			return nil, fmt.Errorf("'%s' must be a framework or service plugin", tag)
		}
		names[i], plugins[i], tags[i] = n, p, p.Tag
	}
	if framework == nil {
		return nil, fmt.Errorf("No framework plugin specified")
	}

	// Generate shared secret for application. The shared secret is a simple
	// mechanism for a scalable application to communicate securely between
	// containers, or used as a randomize seed to generate shared tokens.
	opts.Secret, err = generateSharedSecret()
	if err != nil {
		return nil, err
	}

	// cleanup on failure
	var success bool
	var namespaceCreated, repoCreated bool
	defer func() {
		if !success {
			for _, c := range containers {
				c.Destroy(br.ctx)
			}
			if repoCreated {
				br.SCM.RemoveRepo(opts.Namespace, opts.Name)
			}
			if namespaceCreated {
				br.SCM.RemoveNamespace(opts.Namespace)
			}
		}
	}()

	// check namespace:
	//   namespace cannot be empty
	//   if namespace not exists then create one
	if user.Namespace == "" && opts.Namespace == "" {
		return nil, NoNamespaceError(user.Name)
	}
	if user.Namespace == "" {
		err = br.CreateNamespace(opts.Namespace)
		if err != nil {
			return
		}
		namespaceCreated = true
	} else {
		opts.Namespace = user.Namespace
	}

	// create all containers
	containers, err = br.createContainers(opts, names, plugins)
	if err != nil {
		return
	}

	// create repository for the application
	err = br.SCM.CreateRepo(opts.Namespace, opts.Name)
	if err != nil {
		return
	}
	repoCreated = true

	// populate and deploy application
	if err = populateRepo(br.SCM, &opts, framework); err != nil {
		return
	}
	if err = br.SCM.Deploy(opts.Namespace, opts.Name, "", containers...); err != nil {
		return
	}

	// add application to the user database
	apps[opts.Name] = &userdb.Application{
		CreatedAt: time.Now(),
		Plugins:   tags,
		Secret:    opts.Secret,
	}
	err = br.Users.Update(user.Name, userdb.Args{"applications": apps})
	if err != nil {
		return
	}

	success = true
	return
}

func populateRepo(scm scm.SCM, opts *container.CreateOptions, framework *manifest.Plugin) error {
	if strings.ToLower(opts.Repo) == "empty" {
		return nil
	} else if opts.Repo == "" {
		return populateFromTemplate(scm, opts, filepath.Join(framework.Path, "template"))
	} else {
		return scm.PopulateURL(opts.Namespace, opts.Name, opts.Repo)
	}
}

func populateFromTemplate(scm scm.SCM, opts *container.CreateOptions, template string) error {
	if fi, err := os.Stat(template); err != nil || !fi.IsDir() {
		return nil
	}

	f, err := files.TempFile("", "repo", ".tar")
	if err != nil {
		return err
	}
	defer func() {
		f.Close()
		os.Remove(f.Name())
	}()

	tw := tar.NewWriter(f)
	if err = archive.CopyFileTree(tw, "", template, nil, false); err != nil {
		return err
	}
	tw.Close()

	size, err := f.Seek(0, os.SEEK_CUR)
	f.Seek(0, os.SEEK_SET)
	if err == nil {
		err = scm.Populate(opts.Namespace, opts.Name, f, size)
	}
	return err
}

func generateSharedSecret() (string, error) {
	hash := sha1.New()
	_, err := io.CopyN(hash, rand.Reader, 256*1024)
	if err != nil {
		return "", err
	}
	return hex.EncodeToString(hash.Sum(nil)), nil
}

func (br *UserBroker) CreateServices(opts container.CreateOptions, tags []string) (containers []*container.Container, err error) {
	if err = br.Refresh(); err != nil {
		return nil, err
	}

	user := br.User.Basic()
	app := user.Applications[opts.Name]

	if app == nil {
		return nil, ApplicationNotFoundError(opts.Name)
	}

	// check service plugins
	var (
		names   = make([]string, len(tags))
		plugins = make([]*manifest.Plugin, len(tags))
	)
	for i, tag := range tags {
		n, p, err := br.getPluginInfoWithNames(tag)
		if err != nil {
			return nil, err
		}
		if !p.IsService() {
			return nil, fmt.Errorf("'%s' is not a service plugin", tag)
		}
		names[i], plugins[i], tags[i] = n, p, p.Tag
	}

	opts.Namespace = user.Namespace
	opts.Secret = app.Secret
	opts.Hosts = app.Hosts

	containers, err = br.createContainers(opts, names, plugins)
	if err != nil {
		return nil, err
	}

	app.Plugins = append(app.Plugins, tags...)
	err = br.Users.Update(user.Name, userdb.Args{"applications": user.Applications})
	return containers, err
}

func (br *UserBroker) createContainers(opts container.CreateOptions, serviceNames []string, plugins []*manifest.Plugin) (containers []*container.Container, err error) {
	for i, plugin := range plugins {
		opts.Plugin = plugin
		opts.ServiceName = serviceNames[i]
		var cs []*container.Container
		cs, err = br.Create(br.ctx, opts)
		containers = append(containers, cs...)
		if err != nil {
			return
		}
	}
	return
}

func (br *UserBroker) RemoveApplication(name string) (err error) {
	if err = br.Refresh(); err != nil {
		return err
	}

	user := br.User.Basic()
	apps := user.Applications

	if apps[name] == nil {
		return ApplicationNotFoundError(name)
	}

	var errors errors.Errors

	// remove application containers
	var containers []*container.Container
	containers, err = br.FindAll(br.ctx, name, user.Namespace)
	if err != nil {
		errors.Add(err)
	} else {
		for _, c := range containers {
			errors.Add(c.Destroy(br.ctx))
		}
	}

	// remove application repository
	errors.Add(br.SCM.RemoveRepo(user.Namespace, name))

	// remove application from user database
	delete(apps, name)
	errors.Add(br.Users.Update(user.Name, userdb.Args{"applications": apps}))

	return errors.Err()
}

func (br *UserBroker) RemoveService(name, service string) (err error) {
	if err = br.Refresh(); err != nil {
		return err
	}

	user := br.User.Basic()
	app := user.Applications[name]

	if app == nil {
		return ApplicationNotFoundError(name)
	}

	var errors errors.Errors
	var containers []*container.Container

	containers, err = br.FindService(br.ctx, name, user.Namespace, service)
	if err != nil {
		return err
	}
	if len(containers) == 0 {
		return fmt.Errorf("service '%s' not found in application '%s'", service, name)
	}

	for _, c := range containers {
		errors.Add(c.Destroy(br.ctx))

		tag := c.PluginTag()
		for i := range app.Plugins {
			if tag == app.Plugins[i] {
				app.Plugins = append(app.Plugins[:i], app.Plugins[i+1:]...)
				break
			}
		}
	}

	errors.Add(br.Users.Update(user.Name, userdb.Args{"applications": user.Applications}))
	return errors.Err()
}

type ScalingError int

func (e ScalingError) Error() string {
	return fmt.Sprintf("The scaling number must be between 1 and 10, but given %d", int(e))
}

func (e ScalingError) HTTPErrorStatusCode() int {
	return http.StatusBadRequest
}

// Scale application by adding or removing containers in the application.
func (br *UserBroker) ScaleApplication(name string, num int) ([]*container.Container, error) {
	if num <= 0 || num > 10 {
		return nil, ScalingError(num)
	}

	if err := br.Refresh(); err != nil {
		return nil, err
	}

	user := br.User.Basic()
	app := user.Applications[name]

	if app == nil {
		return nil, ApplicationNotFoundError(name)
	}
	cs, err := br.FindApplications(br.ctx, name, user.Namespace)
	if err != nil {
		return nil, err
	}
	if len(cs) == 0 {
		return nil, ApplicationNotFoundError(name)
	}

	if len(cs) < num {
		return br.scaleUp(cs[0], num, app.Secret, app.Hosts)
	} else if len(cs) > num {
		return nil, br.scaleDown(cs, len(cs)-num)
	} else {
		return nil, nil
	}
}

func (br *UserBroker) scaleUp(replica *container.Container, num int, secret string, hosts []string) ([]*container.Container, error) {
	meta, err := br.Hub.GetPluginInfo(replica.PluginTag())
	if err != nil {
		return nil, err
	}

	opts := container.CreateOptions{
		Name:      replica.Name,
		Namespace: replica.Namespace,
		Hosts:     hosts,
		Plugin:    meta,
		Home:      replica.Home(),
		User:      replica.User(),
		Secret:    secret,
		Scaling:   num,
	}

	containers, err := br.Create(br.ctx, opts)
	if err == nil {
		err = br.SCM.Deploy(opts.Namespace, opts.Name, "", containers...)
	}
	return containers, err
}

func (br *UserBroker) scaleDown(containers []*container.Container, num int) error {
	for i := 0; i < num; i++ {
		if err := containers[i].Destroy(br.ctx); err != nil {
			return err
		}
	}
	return nil
}

func (br *UserBroker) AddHost(name, host string) error {
	if host == "" || strings.HasSuffix(host, "."+defaults.Domain()) {
		return errs.New("Invalid domain name")
	}

	if err := br.Refresh(); err != nil {
		return err
	}

	user := br.User.Basic()
	app := user.Applications[name]
	if app == nil {
		return ApplicationNotFoundError(name)
	}

	for _, h := range app.Hosts {
		if host == h {
			return nil
		}
	}

	cs, err := br.FindAll(br.ctx, name, user.Namespace)
	if err != nil {
		return err
	}

	for _, c := range cs {
		if c.Category().IsFramework() {
			c.AddHost(br.ctx, host)
		} else if c.Category().IsService() {
			c.AddHost(br.ctx, c.ServiceName()+"."+host)
		}
	}

	app.Hosts = append(app.Hosts, host)
	return br.Users.Update(user.Name, userdb.Args{"applications": user.Applications})
}

func (br *UserBroker) RemoveHost(name, host string) error {
	if err := br.Refresh(); err != nil {
		return err
	}

	user := br.User.Basic()
	app := user.Applications[name]
	if app == nil {
		return ApplicationNotFoundError(name)
	}

	var removed bool
	for i, h := range app.Hosts {
		if host == h {
			app.Hosts = append(app.Hosts[:i], app.Hosts[i+1:]...)
			removed = true
		}
	}
	if !removed {
		return nil
	}

	cs, err := br.FindAll(br.ctx, name, user.Namespace)
	if err != nil {
		return err
	}

	for _, c := range cs {
		if c.Category().IsFramework() {
			c.RemoveHost(br.ctx, host)
		} else if c.Category().IsService() {
			c.RemoveHost(br.ctx, c.ServiceName()+"."+host)
		}
	}

	return br.Users.Update(user.Name, userdb.Args{"applications": user.Applications})
}

func (br *UserBroker) StartApplication(name string) error {
	return br.startApplication(name, func(c *container.Container) error { return c.Start(br.ctx) })
}

func (br *UserBroker) RestartApplication(name string) error {
	return br.startApplication(name, func(c *container.Container) error { return c.Restart(br.ctx) })
}

func (br *UserBroker) startApplication(name string, fn func(*container.Container) error) error {
	containers, err := br.FindAll(br.ctx, name, br.Namespace())
	if err != nil {
		return err
	}
	if len(containers) == 0 {
		return ApplicationNotFoundError(name)
	}
	return startContainers(containers, fn)
}

func (br *UserBroker) StopApplication(name string) error {
	containers, err := br.FindAll(br.ctx, name, br.Namespace())
	if err != nil {
		return err
	}
	if len(containers) == 0 {
		return ApplicationNotFoundError(name)
	}
	return runParallel(err, containers, func(c *container.Container) error { return c.Stop(br.ctx) })
}

func (br *Broker) StartContainers(ctx context.Context, containers []*container.Container) error {
	return startContainers(containers, func(c *container.Container) error { return c.Start(ctx) })
}

func startContainers(containers []*container.Container, fn func(*container.Container) error) error {
	err := container.ResolveServiceDependencies(containers)
	if err != nil {
		return err
	}

	sch := makeSchedule(containers)
	err = runParallel(nil, sch.parallel, fn)
	err = runSerial(err, sch.serial, fn)
	err = runParallel(err, sch.final, fn)
	return err
}

type schedule struct {
	parallel []*container.Container
	serial   []*container.Container
	final    []*container.Container
}

func makeSchedule(containers []*container.Container) *schedule {
	sch := &schedule{}
	for _, c := range containers {
		if c.Category().IsService() {
			if len(c.DependsOn()) == 0 {
				sch.parallel = append(sch.parallel, c)
			} else {
				sch.serial = append(sch.serial, c)
			}
		} else {
			sch.final = append(sch.final, c)
		}
	}
	return sch
}

func runParallel(err error, cs []*container.Container, fn func(*container.Container) error) error {
	if err != nil {
		return err
	}
	if len(cs) == 0 {
		return nil
	}
	if len(cs) == 1 {
		return fn(cs[0])
	}

	var wg sync.WaitGroup
	wg.Add(len(cs))

	var errors errors.Errors
	var errLock sync.Mutex

	for _, c := range cs {
		go func(wg *sync.WaitGroup, c *container.Container) {
			defer wg.Done()
			if err := fn(c); err != nil {
				errLock.Lock()
				errors.Add(err)
				errLock.Unlock()
			}
		}(&wg, c)
	}

	wg.Wait()
	return errors.Err()
}

func runSerial(err error, cs []*container.Container, fn func(*container.Container) error) error {
	if err == nil {
		for _, c := range cs {
			if err = fn(c); err != nil {
				break
			}
		}
	}
	return err
}

// Download application repository as a archive file.
func (br *UserBroker) Download(name string) (io.ReadCloser, error) {
	containers, err := br.FindApplications(br.ctx, name, br.Namespace())
	if err != nil {
		return nil, err
	}
	if len(containers) == 0 {
		return nil, ApplicationNotFoundError(name)
	}
	c := containers[0]
	r, _, err := c.CopyFromContainer(br.ctx, c.ID, c.RepoDir()+"/.")
	return r, err
}

// Upload application repository from a archive file.
func (br *UserBroker) Upload(name string, content io.Reader) error {
	// create a temporary directory to hold deployment archive
	tempdir, err := ioutil.TempDir("", "deploy")
	if err != nil {
		return err
	}
	defer os.RemoveAll(tempdir)

	// save archive to a temporary file
	tempfilename := filepath.Base(tempdir) + ".tar.gz"
	tempfile, err := os.Create(filepath.Join(tempdir, tempfilename))
	if err != nil {
		return err
	}

	_, err = io.Copy(tempfile, content)
	tempfile.Close()
	if err != nil {
		return err
	}

	// deploy to containers
	containers, err := br.FindApplications(br.ctx, name, br.Namespace())
	if err != nil {
		return err
	}
	if len(containers) == 0 {
		return ApplicationNotFoundError(name)
	}
	for _, c := range containers {
		err = c.Deploy(br.ctx, tempdir)
		if err != nil {
			return err
		}
	}
	return nil
}

func (br *UserBroker) Dump(name string) (io.ReadCloser, error) {
	// find all containers
	containers, err := br.FindAll(br.ctx, name, br.Namespace())
	if err != nil {
		return nil, err
	}
	if len(containers) == 0 {
		return nil, ApplicationNotFoundError(name)
	}
	container.ResolveServiceDependencies(containers)

	// create temporary directory to hold snapshot archives
	tempdir, err := ioutil.TempDir("", "snapshot")
	if err != nil {
		return nil, err
	}
	defer os.RemoveAll(tempdir)

	// save snapshot archives
	for _, c := range containers {
		if c.Category().IsFramework() {
			err = saveSnapshot(br.ctx, c, filepath.Join(tempdir, "app", "data.tar"))
		} else if c.Category().IsService() {
			err = saveSnapshot(br.ctx, c, filepath.Join(tempdir, "services", c.ServiceName()+".tar"))
		}
		if err != nil {
			return nil, err
		}
	}

	// create final snapshot archive
	tempfile, err := ioutil.TempFile("", "snapshot")
	if err != nil {
		return nil, err
	}

	tw := tar.NewWriter(tempfile)
	err = archive.CopyFileTree(tw, "", tempdir, nil, false)
	if err == nil {
		err = tw.Close()
	}
	if err != nil {
		tempfile.Close()
		os.Remove(tempfile.Name())
		return nil, err
	}

	tempfile.Seek(0, os.SEEK_SET)
	return deleteReadCloser{tempfile}, nil
}

func (br *UserBroker) Restore(name string, source io.Reader) error {
	// find all containers
	containers, err := br.FindAll(br.ctx, name, br.Namespace())
	if err != nil {
		return err
	}
	if len(containers) == 0 {
		return ApplicationNotFoundError(name)
	}
	container.ResolveServiceDependencies(containers)

	// create temporary directory to hold snapshot archives
	tempdir, err := ioutil.TempDir("", "snapshot")
	if err != nil {
		return err
	}
	defer os.RemoveAll(tempdir)

	// extract snapshot archives
	zr, err := gzip.NewReader(source)
	if err != nil {
		return err
	}
	err = archive.ExtractFiles(tempdir, zr)
	if err != nil {
		return err
	}

	// restore snapshot archive to containers
	for _, c := range containers {
		if c.Category().IsFramework() {
			err = restoreSnapshot(br.ctx, c, filepath.Join(tempdir, "app", "data.tar"))
		} else if c.Category().IsService() {
			err = restoreSnapshot(br.ctx, c, filepath.Join(tempdir, "services", c.ServiceName()+".tar"))
		}
		if err != nil {
			logrus.WithError(err).Warn("Failed to restore snapshot")
		}
	}

	return nil
}

func saveSnapshot(ctx context.Context, c *container.Container, filename string) error {
	if _, err := os.Stat(filename); err == nil {
		return nil // file exists, don't overwrite
	}
	os.MkdirAll(filepath.Dir(filename), 0755)
	file, err := os.Create(filename)
	if err != nil {
		return err
	}
	defer file.Close()
	return c.ExecE(ctx, "", nil, file, "cwctl", "dump")
}

func restoreSnapshot(ctx context.Context, c *container.Container, filename string) error {
	file, err := os.Open(filename)
	if err != nil {
		return err
	}
	defer file.Close()
	return c.ExecE(ctx, "", file, nil, "cwctl", "restore")
}

type deleteReadCloser struct {
	*os.File
}

func (r deleteReadCloser) Close() error {
	if err := r.File.Close(); err != nil {
		return err
	}
	return os.Remove(r.File.Name())
}

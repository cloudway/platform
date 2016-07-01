package broker

import (
    "fmt"
    "time"
    errs "errors"
    "strings"
    "sync"
    "github.com/cloudway/platform/container"
    "github.com/cloudway/platform/auth/userdb"
    "github.com/cloudway/platform/pkg/errors"
    "github.com/cloudway/platform/pkg/manifest"
    "github.com/cloudway/platform/container/conf/defaults"
)

func (br *UserBroker) CreateApplication(opts container.CreateOptions, tags []string) (containers []*container.Container, err error) {
    var (
        user = br.User.Basic()
        apps = user.Applications
        namespaceCreated, repoCreated bool
    )

    // check if the application already exists
    if apps[opts.Name] != nil {
        return nil, ApplicationExistError{opts.Name, user.Namespace}
    }

    // check plugins
    plugins := make([]*manifest.Plugin, len(tags))
    var framework *manifest.Plugin
    for i, tag := range tags {
        p, err := br.Hub.GetPluginInfo(tag)
        if err != nil {
            return nil, err
        }
        if p.IsFramework() {
            if framework != nil {
                return nil, fmt.Errorf("Multiple framework plugins specified: %s and %s", p.Name, framework.Name)
            }
            framework = p
        } else if (!p.IsService()) {
            return nil, fmt.Errorf("'%s' must be a framework or service plugin", tag)
        }
        plugins[i] = p
        tags[i] = p.Name + ":" + p.Version
    }
    if framework == nil {
        return nil, fmt.Errorf("No framework plugin specified")
    }

    // cleanup on failure
    var success bool
    defer func() {
        if !success {
            for _, c := range containers {
                c.Destroy()
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

    // create repository for the application
    err = br.SCM.CreateRepo(opts.Namespace, opts.Name)
    if err != nil {
        return
    }
    repoCreated = true

    // create all containers
    containers, err = br.createContainers(opts, plugins)
    if err != nil {
        return
    }

    // add application to the user database
    apps[opts.Name] = &userdb.Application{CreatedAt: time.Now(), Plugins: tags}
    err = br.Users.Update(user.Name, userdb.Args{"applications": apps})
    if err != nil {
        return
    }

    success = true
    return
}

func (br *UserBroker) CreateServices(opts container.CreateOptions, tags []string) (containers []*container.Container, err error) {
    user := br.User.Basic()
    app  := user.Applications[opts.Name]

    if app == nil {
        return nil, ApplicationNotFoundError(opts.Name)
    }

    // check service plugins
    plugins := make([]*manifest.Plugin, len(tags))
    for i, tag := range tags {
        p, err := br.Hub.GetPluginInfo(tag)
        if err != nil {
            return nil, err
        }
        if !p.IsService() {
            return nil, fmt.Errorf("'%s' is not a service plugin", tag)
        }
        plugins[i] = p
        tags[i] = p.Name + ":" + p.Version
    }

    opts.Namespace = user.Namespace
    opts.Hosts = app.Hosts
    containers, err = br.createContainers(opts, plugins)
    if err != nil {
        return nil, err
    }

    app.Plugins = append(app.Plugins, tags...)
    err = br.Users.Update(user.Name, userdb.Args{"applications": user.Applications})
    return containers, err
}

func (br *UserBroker) createContainers(opts container.CreateOptions, plugins []*manifest.Plugin) (containers []*container.Container, err error) {
    for _, plugin := range plugins {
        opts.Plugin = plugin
        var cs []*container.Container
        cs, err = br.Create(br.SCM, opts)
        containers = append(containers, cs...)
        if err != nil {
            return
        }
    }
    return
}

func (br *UserBroker) RemoveApplication(name string) (err error) {
    user := br.User.Basic()
    apps := user.Applications

    if apps[name] == nil {
        return ApplicationNotFoundError(name)
    }

    var errors errors.Errors

    // remove application containers
    var containers []*container.Container
    containers, err = br.FindAll(name, user.Namespace)
    if err != nil {
        errors.Add(err)
    } else {
        for _, c := range containers {
            errors.Add(c.Destroy())
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
    user := br.User.Basic()
    app  := user.Applications[name]

    if app == nil {
        return ApplicationNotFoundError(name)
    }

    var errors errors.Errors
    var containers []*container.Container

    containers, err = br.FindService(name, user.Namespace, service)
    if err != nil {
        return err
    }

    for _, c := range containers {
        errors.Add(c.Destroy())

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

// Scale application by adding or removing containers in the application.
func (br *UserBroker) ScaleApplication(name string, num int) ([]*container.Container, error) {
    if num <= 0 || num > 10 {
        return nil, errs.New("The scaling number must be between 1 and 10")
    }

    user := br.User.Basic()
    app  := user.Applications[name]

    if app == nil {
        return nil, ApplicationNotFoundError(name)
    }
    cs, err := br.FindApplications(name, user.Namespace)
    if err != nil {
        return nil, err
    }
    if len(cs) == 0 {
        return nil, ApplicationNotFoundError(name)
    }

    if len(cs) < num {
        return br.scaleUp(cs[0], num, app.Hosts)
    } else if len(cs) > num {
        return nil, br.scaleDown(cs, len(cs)-num)
    } else {
        return nil, nil
    }
}

func (br *UserBroker) scaleUp(replica *container.Container, num int, hosts []string) ([]*container.Container, error) {
    meta, err := br.Hub.GetPluginInfo(replica.PluginTag())
    if err != nil {
        return nil, err
    }

    opts := container.CreateOptions{
        Name:       replica.Name,
        Namespace:  replica.Namespace,
        Hosts:      hosts,
        Plugin:     meta,
        Home:       replica.Home(),
        User:       replica.User(),
        Scaling:    num,
        Repo:       "empty",
    }

    return br.Create(br.SCM, opts)
}

func (br *UserBroker) scaleDown(containers []*container.Container, num int) error {
    for i := 0; i<num; i++ {
        if err := containers[i].Destroy(); err != nil {
            return err
        }
    }
    return nil
}

func (br *UserBroker) AddHost(name, host string) error {
    if host == "" || strings.HasSuffix(host, "."+defaults.Domain()) {
        return errs.New("Invalid domain name")
    }

    user := br.User.Basic()
    app  := user.Applications[name]
    if app == nil {
        return ApplicationNotFoundError(name)
    }

    for _, h := range app.Hosts {
        if host == h {
            return nil
        }
    }

    cs, err := br.FindAll(name, user.Namespace)
    if err != nil {
        return err
    }

    for _, c := range cs {
        if c.Category().IsFramework() {
            c.AddHost(host)
        } else if (c.Category().IsService()) {
            c.AddHost(c.ServiceName() + "." + host)
        }
    }

    app.Hosts = append(app.Hosts, host)
    return br.Users.Update(user.Name, userdb.Args{"applications": user.Applications})
}

func (br *UserBroker) RemoveHost(name, host string) error {
    user := br.User.Basic()
    app  := user.Applications[name]
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

    cs, err := br.FindAll(name, user.Namespace)
    if err != nil {
        return err
    }

    for _, c := range cs {
        if c.Category().IsFramework() {
            c.RemoveHost(host)
        } else if (c.Category().IsService()) {
            c.RemoveHost(c.ServiceName() + "." + host)
        }
    }

    return br.Users.Update(user.Name, userdb.Args{"applications": user.Applications})
}

func (br *UserBroker) StartApplication(name string) error {
    return br.startApplication(name, (*container.Container).Start)
}

func (br *UserBroker) RestartApplication(name string) error {
    return br.startApplication(name, (*container.Container).Restart)
}

func (br *UserBroker) StopApplication(name string) error {
    user := br.User.Basic()
    app := user.Applications[name]
    if app == nil {
        return ApplicationNotFoundError(name)
    }

    containers, err := br.FindAll(name, user.Namespace)
    return startParallel(err, containers, (*container.Container).Stop)
}

func (br *Broker) StartContainers(containers []*container.Container) error {
    return startContainers(containers, (*container.Container).Start)
}

func (br *UserBroker) startApplication(name string, fn func(*container.Container) error) error {
    user := br.User.Basic()
    app := user.Applications[name]
    if app == nil {
        return ApplicationNotFoundError(name)
    }

    containers, err := br.FindAll(name, user.Namespace)
    if err != nil {
        return err
    }
    return startContainers(containers, fn)
}

func startContainers(containers []*container.Container, fn func(*container.Container) error) error {
    err := container.ResolveServiceDependencies(containers)
    if err != nil {
        return err
    }

    sch := makeSchedule(containers)
    err = startParallel(nil, sch.parallel, fn)
    err = startSerial(err, sch.serial, fn)
    err = startParallel(err, sch.final, fn)
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

func startParallel(err error, cs []*container.Container, fn func(*container.Container) error) error {
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

func startSerial(err error, cs []*container.Container, fn func(*container.Container) error) error {
    if err == nil {
        for _, c := range cs {
            if err = fn(c); err != nil {
                break
            }
        }
    }
    return err
}

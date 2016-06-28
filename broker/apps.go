package broker

import (
    "fmt"
    "time"
    "github.com/cloudway/platform/container"
    "github.com/cloudway/platform/auth/userdb"
    "github.com/cloudway/platform/pkg/errors"
    "github.com/cloudway/platform/pkg/manifest"
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

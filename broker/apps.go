package broker

import (
    "github.com/cloudway/platform/container"
    "github.com/cloudway/platform/auth/userdb"
    "github.com/cloudway/platform/pkg/errors"
)

func (br *UserBroker) CreateApplication(opts container.CreateOptions, plugins []string) (containers []*container.Container, err error) {
    var (
        user = br.User.Basic()
        apps = user.Applications
        namespaceCreated, repoCreated bool
    )

    // check if the application already exists
    if apps[opts.Name] != nil {
        return nil, ApplicationExistError{opts.Name, user.Namespace}
    }

    // cleanup on failure
    defer func() {
        if err != nil {
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
    apps[opts.Name] = &userdb.Application{}
    err = br.Users.Update(user.Name, userdb.Args{"applications": apps})
    return
}

func (br *UserBroker) CreateServices(opts container.CreateOptions, plugins []string) (containers []*container.Container, err error) {
    user := br.User.Basic()

    if user.Applications[opts.Name] == nil {
        return nil, ApplicationNotFoundError(opts.Name)
    }

    opts.Namespace = user.Namespace
    return br.createContainers(opts, plugins)
}

func (br *UserBroker) createContainers(opts container.CreateOptions, plugins []string) (containers []*container.Container, err error) {
    for _, plugin := range plugins {
        var cs []*container.Container
        opts.PluginPath, err = br.Hub.GetPluginPath(plugin)
        if err == nil {
            cs, err = br.Create(br.SCM, opts)
            containers = append(containers, cs...)
        }
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

    if user.Applications[name] == nil {
        return ApplicationNotFoundError(name)
    }

    var errors errors.Errors
    var containers []*container.Container
    containers, err = br.FindService(name, user.Namespace, service)
    if err != nil {
        errors.Add(err)
    } else {
        for _, c := range containers {
            errors.Add(c.Destroy())
        }
    }
    return errors.Err()
}

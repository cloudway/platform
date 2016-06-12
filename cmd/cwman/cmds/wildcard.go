package cmds

import (
    "fmt"
    "regexp"
    "github.com/cloudway/platform/container"
)

var reNamePattern = regexp.MustCompile(`^((\*|[a-z][a-z_0-9]*)\.)?([a-z][a-z_0-9]*)-([a-z][a-z_0-9]*)$`)

func splitContainerName(name string) (string, string, string) {
    m := reNamePattern.FindStringSubmatch(name)
    if m != nil && len(m) != 0 {
        return m[2], m[3], m[4]
    } else {
        return "", "", ""
    }
}

func runContainerAction(id string, action func (*container.Container) error) error {
    service, name, namespace := splitContainerName(id)
    if name != "" && namespace != "" {
        var containers []*container.Container
        var err error

        if service == "" {
            // assume the key is 'name-namespace'
            containers, err = container.FindApplications(name, namespace)
        } else if service == "*" {
            // assume the key is '*.name-namespace'
            containers, err = container.FindAll(name, namespace)
            if err == nil {
                err = container.ResolveServiceDependencies(containers)
            }
        } else {
            // assume the key is 'service.name-namespace'
            containers, err = container.FindService(name, namespace, service)
        }

        if err != nil {
            return err
        }
        if len(containers) == 0 {
            return fmt.Errorf("%s: Not found", id)
        }

        for _, c := range containers {
            if err = action(c); err != nil {
                return err
            }
        }
        return nil
    } else {
        // assume the key is an application id
        c, err := container.FromId(id)
        if err != nil {
            return err
        }
        return action(c)
    }
}

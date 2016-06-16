package cmds

import (
    "fmt"
    "github.com/cloudway/platform/container"
)

func (cli *CWMan) runContainerAction(id string, action func (*container.Container) error) error {
    service, name, namespace := container.SplitNames(id)
    if name != "" && namespace != "" {
        var containers []*container.Container
        var err error

        if service == "" {
            // assume the key is 'name-namespace'
            containers, err = cli.FindApplications(name, namespace)
        } else if service == "*" {
            // assume the key is '*.name-namespace'
            containers, err = cli.FindAll(name, namespace)
            if err == nil {
                err = container.ResolveServiceDependencies(containers)
            }
        } else {
            // assume the key is 'service.name-namespace'
            containers, err = cli.FindService(name, namespace, service)
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
        c, err := cli.Inspect(id)
        if err != nil {
            return err
        }
        return action(c)
    }
}

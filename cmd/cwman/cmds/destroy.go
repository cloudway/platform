package cmds

import (
    "fmt"
    "github.com/cloudway/platform/pkg/mflag"
    "github.com/cloudway/platform/container"
    "github.com/cloudway/platform/api/server/runtime"
)

func (cli *CWMan) CmdDestroy(args ...string) error {
    cmd := cli.Subcmd("destroy", "CONTAINER [CONTAINER...]", "all")
    cmd.Require(mflag.Min, 1)
    cmd.ParseFlags(args, true)

    rt, err := runtime.New(cli.DockerClient)
    if err != nil {
        return err
    }

    if len(args) == 1 && args[0] == "all" {
        return destroyAll(rt)
    } else {
        for _, id := range args {
            err := destroy(rt, id)
            if err != nil {
                return err
            }
        }
    }
    return nil
}

func destroyAll(rt *runtime.Runtime) (err error) {
    containers, err := rt.ListAll()
    if err != nil {
        return err
    }

    repoRemoved := make(map[string]bool)
    for _, c := range containers {
        if err = c.Destroy(); err != nil {
            return err
        }

        key := c.Name + "-" + c.Namespace
        if !repoRemoved[key] {
            err = rt.SCM.RemoveRepo(c.Namespace, c.Name)
            if err != nil {
                return err
            }
            repoRemoved[key] = true
        }
    }

    return nil
}

func destroy(rt *runtime.Runtime, id string) (err error) {
    var containers []*container.Container
    service, name, namespace := container.SplitNames(id)

    if name == "" && namespace == "" {
        c, err := rt.Inspect(id)
        if err == nil {
            containers = []*container.Container{c}
            name, namespace = c.Name, c.Namespace
        }
    } else if service == "" {
        containers, err = rt.FindApplications(name, namespace)
    } else if service == "*" {
        containers, err = rt.FindAll(name, namespace)
    } else {
        containers, err = rt.FindService(name, namespace, service)
    }

    if err != nil {
        return err
    }
    if len(containers) == 0 {
        return fmt.Errorf("%s: Not found", id)
    }

    for _, c := range containers {
        if err = c.Destroy(); err != nil {
            return err
        }
    }

    // remove repository if all containers are removed
    containers, _ = rt.FindAll(name, namespace)
    if len(containers) == 0 {
        err = rt.SCM.RemoveRepo(namespace, name)
        if err != nil {
            return err
        }
    }

    return nil
}
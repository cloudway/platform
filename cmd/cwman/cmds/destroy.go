package cmds

import (
    "github.com/cloudway/platform/pkg/mflag"
    "github.com/cloudway/platform/container"
    "github.com/cloudway/platform/broker"
)

func (cli *CWMan) CmdDestroy(args ...string) error {
    cmd := cli.Subcmd("destroy", "CONTAINER [CONTAINER...]", "all")
    cmd.Require(mflag.Min, 1)
    cmd.ParseFlags(args, true)

    if len(args) == 1 && args[0] == "all" {
        return destroyAll(cli)
    } else {
        for _, id := range args {
            err := destroy(cli, id)
            if err != nil {
                return err
            }
        }
    }
    return nil
}

func destroyAll(cli *CWMan) (err error) {
    containers, err := cli.ListAll()
    if err != nil {
        return err
    }

    apps := make(map[string]bool)
    for _, c := range containers {
        key := c.Name + "-" + c.Namespace
        if !apps[key] {
            br, err := cli.NewUserBrokerFromNamespace(c.Namespace)
            if err == nil {
                err = br.RemoveApplication(c.Name)
            }
            if err != nil {
                return err
            }
            apps[key] = true
        }
    }
    return nil
}

func destroy(cli *CWMan, id string) (err error) {
    service, name, namespace := container.SplitNames(id)

    if name == "" && namespace == "" {
        return broker.ApplicationNotFoundError(id)
    }

    br, err := cli.NewUserBrokerFromNamespace(namespace)
    if err != nil {
        return err
    }

    if service == "" || service == "*" {
        return br.RemoveApplication(name)
    } else {
        return br.RemoveService(name, service)
    }
}

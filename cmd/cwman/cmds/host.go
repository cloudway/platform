package cmds

import (
    "github.com/cloudway/platform/pkg/mflag"
    . "github.com/cloudway/platform/container"
)

func (cli *CWMan) CmdAddHost(args ...string) error {
    cmd := cli.Subcmd("addhost", "CONTAINER HOSTNAME [HOSTNAME...]")
    cmd.Require(mflag.Min, 2)
    cmd.ParseFlags(args, true)
    return modifyExtraHosts(cmd.Args(), (*Container).AddHost)
}

func (cli *CWMan) CmdRemoveHost(args ...string) error {
    cmd := cli.Subcmd("rmhost", "CONTAINER HOSTNAME [HOSTNAME...]")
    cmd.Require(mflag.Min, 2)
    cmd.ParseFlags(args, true)
    return modifyExtraHosts(cmd.Args(), (*Container).RemoveHost)
}

type hostAction func(*Container, string, ...string) error

func modifyExtraHosts(args []string, action hostAction) error {
    service, name, namespace := splitContainerName(args[0])
    hosts := args[1:]

    if name == "" || namespace == "" {
        return modifyContainerHosts(args[0], hosts, action)
    } else if service == "" || service == "*" {
        return modifyApplicationHosts(name, namespace, hosts, action)
    } else {
        return modifyServiceHosts(name, namespace, service, hosts, action)
    }
}

func modifyContainerHosts(id string, hosts []string, action hostAction) error {
    c, err := FromId(id)
    if err != nil {
        return err
    }
    return action(c, hosts[0], hosts[1:]...)
}

func modifyApplicationHosts(name, namespace string, hosts []string, action hostAction) error {
    cs, err := FindAll(name, namespace)
    if err != nil {
        return err
    }
    for _, c := range cs {
        service := c.ServiceName()
        if service != "" {
            shosts := make([]string, len(hosts))
            for i, h := range hosts {
                shosts[i] = service + "." + h
            }
            if err = action(c, shosts[0], shosts[1:]...); err != nil {
                return err
            }
        } else {
            if err = action(c, hosts[0], hosts[1:]...); err != nil {
                return err
            }
        }
    }
    return nil
}

func modifyServiceHosts(name, namespace, service string, hosts []string, action hostAction) error {
    cs, err := FindService(name, namespace, service)
    if err != nil {
        return err
    }
    for _, c := range cs {
        if err = action(c, hosts[0], hosts[1:]...); err != nil {
            return err
        }
    }
    return nil
}

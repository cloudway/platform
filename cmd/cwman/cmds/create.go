package cmds

import (
    "github.com/cloudway/platform/pkg/mflag"
    "errors"
    "github.com/cloudway/platform/container"
)

func (cli *CWMan) CmdCreate(args ...string) error {
    cmd := cli.Subcmd("create", "[SERVICE.]NAME-NAMESPACE PLUGIN")
    user := cmd.String([]string{"u", "-user"}, "", "Specify the username")
    capacity := cmd.String([]string{"c", "-capacity"}, "", "Application capacity (small,medium,large)")
    scale := cmd.Int([]string{"s", "scale"}, 1, "Application scaling")
    cmd.Require(mflag.Exact, 2)
    cmd.ParseFlags(args, true)

    service, name, namespace := splitContainerName(cmd.Arg(0))
    if name == "" || namespace == "" || service == "*" {
        return (errors.New(
            "The name and namespace arguments can only containes " +
            "lower case letters, digits, or underscores. " +
            "The name and namespace arguments must separated by " +
            "a dash (-) character."))
    }

    config := container.CreateOptions{
        Name:           name,
        Namespace:      namespace,
        ServiceName:    service,
        PluginPath:     cmd.Arg(1),
        User:           *user,
        Capacity:       *capacity,
        Scaling:        *scale,
    }

    _, err := container.Create(config)
    return err
}

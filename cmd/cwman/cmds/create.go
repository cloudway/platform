package cmds

import (
    "errors"
    "github.com/cloudway/platform/pkg/mflag"
    . "github.com/cloudway/platform/pkg/opts"
    "github.com/cloudway/platform/container"
    "github.com/cloudway/platform/hub"
)

func (cli *CWMan) CmdCreate(args ...string) error {
    var start bool
    var err error

    opts := container.CreateOptions{}

    cmd := cli.Subcmd("create", "[SERVICE.]NAME-NAMESPACE PLUGIN [PLUGIN...]")
    cmd.StringVar(&opts.User, []string{"u", "-user"}, "", "Specify the username")
    cmd.StringVar(&opts.Capacity, []string{"c", "-capacity"}, "", "Application capacity (small,medium,large)")
    cmd.IntVar(&opts.Scaling, []string{"s", "scale"}, 1, "Application scaling")
    cmd.Var(NewMapOptsRef(&opts.Env, ValidateEnv), []string{"e", "-env"}, "Set environment variables")
    cmd.BoolVar(&start, []string{"-start"}, false, "Start containers after create")
    cmd.Require(mflag.Min, 2)
    cmd.ParseFlags(args, true)

    opts.ServiceName, opts.Name, opts.Namespace = container.SplitNames(cmd.Arg(0))
    if opts.Name == "" || opts.Namespace == "" || opts.ServiceName == "*" {
        return errors.New(
            "The name and namespace arguments can only containes " +
            "lower case letters, digits, or underscores. " +
            "The name and namespace arguments must separated by " +
            "a dash (-) character.")
    }

    hub, err := hub.New()
    if err != nil {
        return err
    }

    var containers []*container.Container
    for i := 1; i < cmd.NArg(); i++ {
        var cs []*container.Container

        opts.PluginPath, err = hub.GetPluginPath(cmd.Arg(i))
        if err == nil {
            cs, err = cli.Create(opts)
        }
        if err != nil {
            for _, c := range containers {
                c.Destroy()
            }
            return err
        }
        containers = append(containers, cs...)
    }

    if start {
        err = container.ResolveServiceDependencies(containers);
        if err != nil {
            return err
        }
        for _, c := range containers {
            if err = c.Start(); err != nil {
                return err
            }
        }
    }

    return nil
}

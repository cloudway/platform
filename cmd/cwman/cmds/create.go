package cmds

import (
    "errors"
    "github.com/cloudway/platform/pkg/mflag"
    . "github.com/cloudway/platform/pkg/opts"
    "github.com/cloudway/platform/container"
    "github.com/cloudway/platform/api/server/runtime"
    "github.com/cloudway/platform/scm"
)

func (cli *CWMan) CmdCreate(args ...string) (err error) {
    var (
        start       bool
        opts        container.CreateOptions
        repoCreated bool
        containers  []*container.Container
    )

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
            "The name and namespace arguments can only contains " +
            "lower case letters, digits, or underscores. " +
            "The name and namespace arguments must separated by " +
            "a dash (-) character.")
    }

    rt, err := runtime.New(cli.DockerClient)
    if err != nil {
        return err
    }

    // cleanup on failure
    defer func() {
        if err != nil {
            for _, c := range containers {
                c.Destroy()
            }
            if repoCreated {
                rt.SCM.RemoveRepo(opts.Namespace, opts.Name)
            }
        }
    }()

    // create repository if needed
    if opts.ServiceName == "" {
        err = rt.SCM.CreateRepo(opts.Namespace, opts.Name)
        if _, ok := err.(scm.RepoExistError); err != nil && !ok {
            return err
        } else {
            err = nil
            repoCreated = true
        }
    }

    // create all containers
    for i := 1; i < cmd.NArg(); i++ {
        var cs []*container.Container
        opts.PluginPath, err = rt.Hub.GetPluginPath(cmd.Arg(i))
        if err == nil {
            cs, err = cli.Create(opts)
        }
        if err != nil {
            return err
        }
        containers = append(containers, cs...)
    }

    // start all containers
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

package cmds

import (
    "os"
    "github.com/cloudway/platform/sandbox"
    "encoding/json"
    "github.com/cloudway/platform/pkg/manifest"
)

func (cli *CWCtl) CmdInfo(args ...string) error {
    if os.Getuid() != 0 {
        return os.ErrPermission
    }

    cmd := cli.Subcmd("info")
    ip := cmd.String([]string{"-ip"}, "", "Set IP address for endpoints")
    cmd.ParseFlags(args, false)

    app := sandbox.NewApplication()
    env := app.ExportedEnviron()

    endpoints, err := app.GetEndpoints(*ip)
    if err != nil {
        return err
    }

    ps, err := app.GetPlugins()
    if err != nil {
        return err
    }
    plugins := make([]*manifest.Plugin, 0, len(ps))
    for _, p := range ps {
        plugins = append(plugins, p)
    }

    info := manifest.ApplicationInfo{
        Env:        env,
        Endpoints:  endpoints,
        Plugins:    plugins,
    }

    return json.NewEncoder(os.Stdout).Encode(&info)
}

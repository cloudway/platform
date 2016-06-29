package cmds

import (
    "os"
    "encoding/json"
    "github.com/cloudway/platform/sandbox"
    "github.com/cloudway/platform/pkg/manifest"
)

func (cli *CWCtl) CmdInfo(args ...string) error {
    if os.Getuid() != 0 {
        return os.ErrPermission
    }

    cmd := cli.Subcmd("info")
    ip := cmd.String([]string{"-ip"}, "", "Set IP address for endpoints")
    cmd.ParseFlags(args, false)

    box := sandbox.New()
    env := box.ExportedEnviron()

    endpoints, err := box.GetEndpoints(*ip)
    if err != nil {
        return err
    }

    ps, err := box.Plugins()
    if err != nil {
        return err
    }
    plugins := make([]*manifest.Plugin, 0, len(ps))
    for _, p := range ps {
        plugins = append(plugins, p)
    }

    info := manifest.SandboxInfo{
        Env:        env,
        Endpoints:  endpoints,
        Plugins:    plugins,
        State:      box.ActiveState(),
    }

    return json.NewEncoder(os.Stdout).Encode(&info)
}

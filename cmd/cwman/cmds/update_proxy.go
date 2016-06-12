package cmds

import (
    "os"
    "github.com/docker/engine-api/client"
    "github.com/cloudway/platform/proxy"
)

func (_ *CWMan) CmdUpdateProxy(args ...string) error {
    cli, err := client.NewEnvClient()
    if err != nil {
        return err
    }
    prx, err := proxy.New(os.Getenv("CLOUDWAY_PROXY_HOST"))
    if err != nil {
        return err
    }
    return proxy.RunUpdater(cli, prx)
}

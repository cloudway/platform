package cmds

import (
    "os"
    "github.com/cloudway/platform/proxy"
)

func (cli *CWMan) CmdUpdateProxy(args ...string) error {
    prx, err := proxy.New(os.Getenv("CLOUDWAY_PROXY_HOST"))
    if err != nil {
        return err
    }
    return proxy.RunUpdater(cli.DockerClient, prx)
}

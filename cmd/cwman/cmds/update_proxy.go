package cmds

import (
	"time"

	"github.com/cloudway/platform/config"
	"github.com/cloudway/platform/proxy"
)

func (cli *CWMan) CmdUpdateProxy(args ...string) (err error) {
	var prx proxy.Proxy

	for i := 0; i < 3; i++ {
		prx, err = proxy.New(config.Get("proxy.url"))
		if err == nil {
			break
		}
		if err == proxy.ErrMisconfigured {
			return err
		}
		time.Sleep(time.Second * 5)
	}

	return proxy.RunUpdater(cli.DockerClient, prx)
}

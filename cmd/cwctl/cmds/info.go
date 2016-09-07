package cmds

import (
	"encoding/json"
	"os"

	"github.com/cloudway/platform/pkg/manifest"
	"github.com/cloudway/platform/sandbox"
)

func (cli *CWCtl) CmdInfo(args ...string) (err error) {
	if os.Getuid() != 0 {
		return os.ErrPermission
	}

	var ip string
	var f_env, f_env_all, f_endpoints, f_plugins, f_state bool
	var f_all bool

	cmd := cli.Subcmd("info")
	cmd.StringVar(&ip, []string{"-ip"}, "", "Set IP address for endpoints")
	cmd.BoolVar(&f_env, []string{"-env"}, false, "Show environment variables")
	cmd.BoolVar(&f_env_all, []string{"-env-all"}, false, "Show all environment variables")
	cmd.BoolVar(&f_endpoints, []string{"-endpoints"}, false, "Show endpoints information")
	cmd.BoolVar(&f_plugins, []string{"-plugins"}, false, "Show plugin information")
	cmd.BoolVar(&f_state, []string{"-state"}, false, "Show active state information")
	cmd.ParseFlags(args, false)

	f_all = !(f_env || f_endpoints || f_plugins || f_state)

	box := sandbox.New()
	info := manifest.SandboxInfo{}

	if f_all || f_env || f_env_all {
		if f_env_all {
			info.Env = box.Environ()
		} else {
			info.Env = box.ExportedEnviron()
		}
	}

	if f_all || f_endpoints {
		info.Endpoints, err = box.GetEndpoints(ip)
		if err != nil {
			return err
		}
	}

	if f_all || f_plugins {
		ps, err := box.Plugins()
		if err != nil {
			return err
		}
		info.Plugins = make([]*manifest.Plugin, 0, len(ps))
		for _, p := range ps {
			info.Plugins = append(info.Plugins, p)
		}
	}

	if f_all || f_state {
		info.State = box.ActiveState()
	}

	return json.NewEncoder(os.Stdout).Encode(&info)
}

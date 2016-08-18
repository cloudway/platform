package cmds

import (
	"github.com/cloudway/platform/container"
	Cli "github.com/cloudway/platform/pkg/cli"
	flag "github.com/cloudway/platform/pkg/mflag"
)

// Command is the struct containing the command name and description
type Command struct {
	Name        string
	Description string
}

type CWMan struct {
	*Cli.Cli
	container.DockerClient
	handlers map[string]func(...string) error
}

// Commands lists the top level commands and their short usage
var CommandUsage = []Command{
	{"api-server", "Start the API server"},
	{"config", "Get or set a configuration value"},
	{"install", "Install one or more plugins"},
	{"run", "Run one-off command in a running container"},
	{"upgrade", "Upgrade application containers"},
	{"useradd", "Add a user"},
	{"userdel", "Remove a user"},
}

var Commands = make(map[string]Command)

func init() {
	for _, cmd := range CommandUsage {
		Commands[cmd.Name] = cmd
	}
}

func Init(docker container.DockerClient) *CWMan {
	cli := new(CWMan)
	cli.Cli = Cli.New("cwman", cli)
	cli.DockerClient = docker
	cli.Description = "Cloudway application container management tool"

	cli.handlers = map[string]func(...string) error{
		"api-server":   cli.CmdAPIServer,
		"update-proxy": cli.CmdUpdateProxy,
		"sshd":         cli.CmdSshd,
		"config":       cli.CmdConfig,
		"install":      cli.CmdInstallPlugin,
		"deploy":       cli.CmdDeploy,
		"run":          cli.CmdRun,
		"upgrade":      cli.CmdUpgrade,
		"useradd":      cli.CmdUserAdd,
		"userdel":      cli.CmdUserDel,
	}

	return cli
}

func (cli *CWMan) Command(name string) func(...string) error {
	return cli.handlers[name]
}

func (cli *CWMan) Subcmd(name string, synopses ...string) *flag.FlagSet {
	var description string
	if cmd, ok := Commands[name]; ok {
		description = cmd.Description
	}
	return cli.Cli.Subcmd(name, synopses, description, true)
}

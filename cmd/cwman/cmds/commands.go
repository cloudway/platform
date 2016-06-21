package cmds

import (
    Cli "github.com/cloudway/platform/pkg/cli"
    flag "github.com/cloudway/platform/pkg/mflag"
    "github.com/cloudway/platform/container"
)

// Command is the struct containing the command name and description
type Command struct {
    Name        string
    Description string
}

type CWMan struct {
    *Cli.Cli
    container.DockerClient
    handlers map[string]func(...string)error
}

// Commands lists the top level commands and their short usage
var CommandUsage = []Command {
    {"create",    "Create a new application container"},
    {"destroy",   "Destroy application containers"},
    {"add",       "Add a plugin to application container"},
    {"list",      "List all application containers"},
    {"start",     "Start one or more stopped containers"},
    {"stop",      "Stop a running container"},
    {"restart",   "Restart a container"},
    {"status",    "Show application container status"},
    {"run",       "Run one-off command in a running container"},
    {"env",       "Show container environment variables"},
    {"addhost",   "Add extra host name to the container"},
    {"rmhost",    "Remove extra host name from the container"},
    {"download",  "Download application files"},
    {"upload",    "Upload files into repo directory"},
    {"useradd",   "Add a user"},
    {"userdel",   "Remove a user"},
    {"usermod",   "Modify a user"},
    {"password",  "Change user password"},
    {"api-server","Start the API server"},
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

    cli.handlers = map[string]func(...string)error {
        "version":      cli.CmdVersion,
        "create":       cli.CmdCreate,
        "destroy":      cli.CmdDestroy,
        "add":          cli.CmdAdd,
        "list":         cli.CmdList,
        "deploy":       cli.CmdDeploy,
        "start":        cli.CmdStart,
        "stop":         cli.CmdStop,
        "restart":      cli.CmdRestart,
        "status":       cli.CmdStatus,
        "run":          cli.CmdRun,
        "env":          cli.CmdEnv,
        "addhost":      cli.CmdAddHost,
        "rmhost":       cli.CmdRemoveHost,
        "download":     cli.CmdDownload,
        "upload":       cli.CmdUpload,

        "install":      cli.CmdInstallPlugin,

        "useradd":      cli.CmdUserAdd,
        "userdel":      cli.CmdUserDel,
        "usermod":      cli.CmdUserMod,
        "password":     cli.CmdPassword,

        "create-namespace": cli.CmdCreateNamespace,
        "remove-namespace": cli.CmdRemoveNamespace,
        "create-repo":      cli.CmdCreateRepo,
        "remove-repo":      cli.CmdRemoveRepo,
        "add-key":          cli.CmdAddKey,
        "remove-key":       cli.CmdRemoveKey,
        "list-keys":        cli.CmdListKeys,

        "api-server":   cli.CmdAPIServer,
        "update-proxy": cli.CmdUpdateProxy,
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

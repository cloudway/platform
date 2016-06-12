package cmds

import (
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
    handlers map[string]func(...string)error
}

// Commands lists the top level commands and their short usage
var CommandUsage = []Command {
    {"create",   "Create a new application container"},
    {"destroy",  "Destroy application containers"},
    {"list",     "List all application containers"},
    {"start",    "Start one or more stopped containers"},
    {"stop",     "Stop a running container"},
    {"restart",  "Restart a container"},
    {"run",      "Run one-off command in a running container"},
    {"env",      "Show container environment variables"},
    {"install",  "Install a plugin to application container"},
    {"download", "Download application files"},
    {"upload",   "Upload files into repo directory"},
}

var Commands = make(map[string]Command)

func init() {
    for _, cmd := range CommandUsage {
        Commands[cmd.Name] = cmd
    }
}

func Init() *CWMan {
    cli := new(CWMan)
    cli.Cli = Cli.New("cwman", cli)
    cli.Description = "Cloudway application container management tool"

    cli.handlers = map[string]func(...string)error {
        "create":   cli.CmdCreate,
        "destroy":  cli.CmdDestroy,
        "list":     cli.CmdList,
        "start":    cli.CmdStart,
        "stop":     cli.CmdStop,
        "restart":  cli.CmdRestart,
        "run":      cli.CmdRun,
        "env":      cli.CmdEnv,
        "install":  cli.CmdInstall,
        "download": cli.CmdDownload,
        "upload":   cli.CmdUpload,
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

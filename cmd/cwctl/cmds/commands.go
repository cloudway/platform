package cmds

import (
    "os"
    Cli "github.com/cloudway/platform/pkg/cli"
    flag "github.com/cloudway/platform/pkg/mflag"
)

// Command is the struct containing the command name and description
type Command struct {
    Name        string
    Description string
}

type CWCtl struct {
    *Cli.Cli
    handlers map[string]func(...string)error
}

// Commands lists the top level commands and their short usage
var CommandUsage = []Command {
    {"start",   "Start the application"},
    {"stop",    "Stop the application"},
    {"restart", "Restart the application"},
    {"status",  "Show application status"},
}

var Commands = make(map[string]Command)

func Init() *CWCtl {
    cli := new(CWCtl)
    cli.Cli = Cli.New("cwctl", cli)
    cli.Description = "Cloudway application control tool"

    cli.handlers = map[string]func(...string)error {
        "start":    cli.CmdStart,
        "stop":     cli.CmdStop,
        "restart":  cli.CmdRestart,
        "status":   cli.CmdStatus,
        "run":      cli.CmdRun,     // HIDDEN
        "sh":       cli.CmdSh,      // HIDDEN
        "pwgen":    cli.CmdPwgen,   // HIDDEN
    }

    if os.Getuid() == 0 {
        cli.handlers["info"]    = cli.CmdInfo
        cli.handlers["setenv"]  = cli.CmdSetenv
        cli.handlers["install"] = cli.CmdInstall
    }

    for _, cmd := range CommandUsage {
        Commands[cmd.Name] = cmd
    }

    return cli
}

func (cli *CWCtl) Command(name string) func(...string) error {
    return cli.handlers[name]
}

func (cli *CWCtl) Subcmd(name string, synopses ...string) *flag.FlagSet {
    var description string
    if cmd, ok := Commands[name]; ok {
        description = cmd.Description
    }
    return cli.Cli.Subcmd(name, synopses, description, true)
}

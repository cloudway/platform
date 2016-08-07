package cmds

import (
    "github.com/cloudway/platform/pkg/cli"
    flag "github.com/cloudway/platform/pkg/mflag"
)

// Command is the struct containing the command name and description
type Command struct {
    Name        string
    Description string
}

type CWCli struct {
    *cli.Cli
    handlers map[string]func(...string)error
}

// Commands lists the top level commands and their short usage
var CommandUsage = []Command {
    {"version", "Show the version information"},
}

var Commands = make(map[string]Command)

func init() {
    for _, cmd := range CommandUsage {
        Commands[cmd.Name] = cmd
    }
}

func Init() *CWCli {
    c := new(CWCli)
    c.Cli = cli.New("cwcli", c)
    c.Description = "Cloudway client interface"

    c.handlers = map[string]func(...string)error {
        "version": c.CmdVersion,
    }

    return c
}

func (c *CWCli) Command(name string) func(...string) error {
    return c.handlers[name]
}

func (c *CWCli) Subcmd(name string, synopses ...string) *flag.FlagSet {
    var description string
    if cmd, ok := Commands[name]; ok {
        description = cmd.Description
    }
    return c.Cli.Subcmd(name, synopses, description, true)
}

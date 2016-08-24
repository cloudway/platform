package cli

import (
	"errors"
	"fmt"
	"io"
	"os"
	"strings"

	flag "github.com/cloudway/platform/pkg/mflag"
)

// Cli represents a command line interface.
type Cli struct {
	Prog        string
	Description string
	Stderr      io.Writer
	handlers    []Handler
	Usage       func()
}

// Handler holds the different commands Cli will call.
// It should have methods with names starting with `Cmd` like:
//  func (h myHandler) CmdFoo(args ...string) error
type Handler interface {
	Command(name string) func(...string) error
}

// Initializer can be optionally implemented by a Handler to
// initialize before each call to one of its commands.
type Initializer interface {
	Initialize() error
}

// New instantiates a ready-to-use Cli
func New(prog string, handlers ...Handler) *Cli {
	// make the generic Cli object that first cli handler
	// in order to handle `help` appropriately
	cli := &Cli{Prog: prog}
	cli.handlers = append([]Handler{cli}, handlers...)
	return cli
}

var errCommandNotFound = errors.New("Command not found")

func (cli *Cli) command(args ...string) (func(...string) error, error) {
	for _, c := range cli.handlers {
		if c == nil {
			continue
		}
		if cmd := c.Command(strings.Join(args, " ")); cmd != nil {
			if ci, ok := c.(Initializer); ok {
				if err := ci.Initialize(); err != nil {
					return nil, err
				}
			}
			return cmd, nil
		}
	}
	return nil, errCommandNotFound
}

// Run executes the specified command
func (cli *Cli) Run(args ...string) error {
	if len(args) > 1 {
		command, err := cli.command(args[:2]...)
		if err == nil {
			return command(args[2:]...)
		}
		if err != errCommandNotFound {
			return err
		}
	}
	if len(args) > 0 {
		command, err := cli.command(args[0])
		if err != nil {
			if err == errCommandNotFound {
				cli.noSuchCommand(args[0])
				return nil
			}
			return err
		}
		return command(args[1:]...)
	}
	return cli.CmdHelp()
}

func (cli *Cli) noSuchCommand(command string) {
	if cli.Stderr == nil {
		cli.Stderr = os.Stderr
	}
	fmt.Fprintf(cli.Stderr, "Error: unknown command %q for %q\nRun '%s --help' for usage.\n", command, cli.Prog, cli.Prog)
	os.Exit(1)
}

// Command returns a command handler, or nil if the command does not exist
func (cli *Cli) Command(name string) func(...string) error {
	if name == "help" {
		return cli.CmdHelp
	} else {
		return nil
	}
}

// CmdHelp displays information on a command.
//
// If more than one command is specified, information is only shown for the first command.
//
// Usage: cli help COMMAND or cli COMMAND --help
func (cli *Cli) CmdHelp(args ...string) error {
	if len(args) > 1 {
		command, err := cli.command(args[:2]...)
		if err == nil {
			command("--help")
			return nil
		}
		if err != errCommandNotFound {
			return err
		}
	}
	if len(args) > 0 {
		command, err := cli.command(args[0])
		if err != nil {
			if err == errCommandNotFound {
				cli.noSuchCommand(args[0])
				return nil
			}
			return err
		}
		command("--help")
		return nil
	}

	if cli.Usage == nil {
		flag.Usage()
	} else {
		cli.Usage()
	}

	return nil
}

// Subcmd is a subcommand of the main command.
// A subcommand represents an action that can be performed
// from the command line.
//
// To see all available subcommands, run "cli --help".
func (cli *Cli) Subcmd(name string, synopses []string, description string, exitOnError bool) *flag.FlagSet {
	var errorHandling flag.ErrorHandling
	if exitOnError {
		errorHandling = flag.ExitOnError
	} else {
		errorHandling = flag.ContinueOnError
	}
	flags := flag.NewFlagSet(name, errorHandling)
	flags.Usage = func() {
		if description != "" {
			flags.ShortUsage()
			flags.PrintDefaults()
		} else {
			fmt.Fprintf(flags.Out(), "%s: unknown command\n", name)
		}
	}

	flags.ShortUsage = func() {
		options := ""
		if flags.FlagCountUndeprecated() > 0 {
			options = " [OPTIONS]"
		}

		if len(synopses) == 0 {
			synopses = []string{""}
		}

		// Allow for multiple command usage synopses.
		for i, synopsis := range synopses {
			var lead string
			if i == 0 {
				// First line needs the word 'Usage'.
				lead = "Usage: "
			} else {
				lead = "       "
			}

			if synopsis != "" {
				synopsis = " " + synopsis
			}

			fmt.Fprintf(flags.Out(), "%s%s %s%s%s\n", lead, cli.Prog, name, options, synopsis)
		}

		fmt.Fprintf(flags.Out(), "\n%s\n", description)
	}

	return flags
}

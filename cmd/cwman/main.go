package main

import (
	"fmt"
	"os"

	"github.com/Sirupsen/logrus"

	"github.com/cloudway/platform/cmd/cwman/cmds"
	"github.com/cloudway/platform/config"
	"github.com/cloudway/platform/container"
	flag "github.com/cloudway/platform/pkg/mflag"
)

func main() {
	stdout := os.Stdout

	err := config.Initialize()
	if err != nil {
		fmt.Fprintln(os.Stderr, err)
		os.Exit(1)
	}

	flag.Usage = func() {
		flag.CommandLine.SetOutput(stdout)

		fmt.Fprint(stdout, "Usage: cwman [OPTIONS] COMMAND [arg...]\n       cwman [ --help ]\n")
		help := "\nCommands:\n\n"

		commands := cmds.CommandUsage
		for _, cmd := range commands {
			help += fmt.Sprintf("  %-12.12s%s\n", cmd.Name, cmd.Description)
		}
		fmt.Fprintf(stdout, "%s\n", help)

		fmt.Fprint(stdout, "Options:\n")
		flag.PrintDefaults()
		fmt.Fprint(stdout, "\nRun 'cwman COMMAND --help' for more information on a command.\n")
	}

	flgHelp := flag.Bool([]string{"h", "-help"}, false, "Print usage")
	flgDebug := flag.Bool([]string{"D", "-debug"}, false, "Debugging mode")

	flag.Parse()

	if *flgHelp {
		// if global flag --help is present, regardless of what other options
		// and commands there are, just print the usage.
		flag.Usage()
		return
	}

	if *flgDebug {
		config.Debug = true
		logrus.SetLevel(logrus.DebugLevel)
	}

	engine, err := container.NewEngine()
	if err != nil {
		fmt.Fprintln(os.Stderr, err)
		os.Exit(1)
	}

	c := cmds.Init(engine)
	if err := c.Run(flag.Args()...); err != nil {
		fmt.Fprintln(os.Stderr, err)
		os.Exit(1)
	}
}

package main

import (
    "os"
    "fmt"
    flag "github.com/cloudway/platform/pkg/mflag"
    "github.com/cloudway/platform/cmd/cwctl/cmds"
    "github.com/Sirupsen/logrus"
)

func main() {
    stdout := os.Stdout

    flag.Usage = func() {
        flag.CommandLine.SetOutput(stdout)

        fmt.Fprint(stdout, "Usage: cwctl [OPTIONS] COMMAND [arg...]\n       cwctl [ --help ]\n")
        help := "\nCommands:\n\n"

        commands := cmds.CommandUsage
        for _, cmd := range commands {
            help += fmt.Sprintf("  %-10.10s%s\n", cmd.Name, cmd.Description)
        }
        fmt.Fprintf(stdout, "%s\n", help)

        fmt.Fprint(stdout, "Options:\n")
        flag.PrintDefaults()
        fmt.Fprint(stdout, "\nRun 'cwctl COMMAND --help' for more information on a command.\n")
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
        logrus.SetLevel(logrus.DebugLevel)
    }

    c := cmds.Init()
    if err := c.Run(flag.Args()...); err != nil {
        fmt.Fprintln(os.Stderr, err)
        os.Exit(1)
    }
}

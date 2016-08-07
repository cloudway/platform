package main

import (
    "os"
    "fmt"
    "github.com/cloudway/platform/config"
    flag "github.com/cloudway/platform/pkg/mflag"
    "github.com/cloudway/platform/cmd/cwcli/cmds"
)

func main() {
    stdout := os.Stdout

    err := config.InitializeClient()
    if err != nil {
        fmt.Fprintln(os.Stderr, err)
    }

    flag.Usage = func() {
        flag.CommandLine.SetOutput(stdout)

        fmt.Fprint(stdout, "Usage: cwcli [OPTIONS] COMMAND [arg...]\n       cwcli [ --help ]\n")
        help := "\nCommands:\n\n"

        commands := cmds.CommandUsage
        for _, cmd := range commands {
            help += fmt.Sprintf("  %-12.12s%s\n", cmd.Name, cmd.Description)
        }
        fmt.Fprintf(stdout, "%s\n", help)

        fmt.Fprint(stdout, "Options:\n")
        flag.PrintDefaults()
        fmt.Fprint(stdout, "\nRun 'cwcli COMMAND --help' for more information on a command.\n")
    }

    flgHelp := flag.Bool([]string{"h", "-help"}, false, "Print usage")

    flag.Parse()

    if *flgHelp {
        // if global flag --help is present, regardless of what other options
        // and commands there are, just print the usage.
        flag.Usage()
        return
    }

    c := cmds.Init()
    if err := c.Run(flag.Args()...); err != nil {
        fmt.Fprintln(os.Stderr, err)
        os.Exit(1)
    }
}

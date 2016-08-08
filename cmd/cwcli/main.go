package main

import (
    "os"
    "fmt"
    "strings"
    "net/url"
    "net/http"

    flag "github.com/cloudway/platform/pkg/mflag"
    "github.com/cloudway/platform/config"
    "github.com/cloudway/platform/cmd/cwcli/cmds"
    "github.com/cloudway/platform/pkg/rest"
)

func main() {
    stdout := os.Stdout

    err := config.InitializeClient()
    if err != nil {
        fmt.Fprintln(os.Stderr, err)
        os.Exit(1)
    }

    flag.Usage = func() {
        flag.CommandLine.SetOutput(stdout)

        fmt.Fprint(stdout, "Usage: cwcli [OPTIONS] COMMAND [arg...]\n       cwcli [ --help ]\n")
        help := "\nCommands:\n\n"

        commands := cmds.CommandUsage
        for _, cmd := range commands {
            if !strings.ContainsRune(cmd.Name, ':') {
                help += fmt.Sprintf("  %-12.12s%s\n", cmd.Name, cmd.Description)
            }
        }
        fmt.Fprintf(stdout, "%s\n", help)

        fmt.Fprint(stdout, "Options:\n")
        flag.PrintDefaults()
        fmt.Fprint(stdout, "\nRun 'cwcli COMMAND --help' for more information on a command.\n")
    }

    flgHelp := flag.Bool([]string{"h", "-help"}, false, "Print usage")
    flgHost := flag.String([]string{"H", "-host"}, "", "Connect to remote host")

    flag.Parse()

    if *flgHelp {
        // if global flag --help is present, regardless of what other options
        // and commands there are, just print the usage.
        flag.Usage()
        return
    }

    host := *flgHost
    if host == "" {
        host = config.Get("host")
    } else if host, err = parseHost(host); err != nil {
        fmt.Fprintln(os.Stderr, err)
        os.Exit(1)
    } else {
        config.Set("host", host)
        config.Save()
    }

    c := cmds.Init(host)
    if err := c.Run(flag.Args()...); err != nil {
        if se, ok := err.(rest.ServerError); ok && se.StatusCode() == http.StatusUnauthorized {
            fmt.Fprintln(os.Stderr, "Your access token has been expired, please login again.")
        } else {
            fmt.Fprintln(os.Stderr, err)
        }
        os.Exit(1)
    }
}

func parseHost(host string) (string, error) {
    if strings.Contains(host, "://") {
        if u, err := url.Parse(host); err != nil {
            return "", err
        } else {
            u.Path = ""
            return u.String(), nil
        }
    } else {
        return "http://" + host, nil
    }
}

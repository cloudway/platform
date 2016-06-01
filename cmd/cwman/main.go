package main

import (
    "fmt"
    "os"
    "github.com/cloudway/platform/cmd/cwman/cmds"
    "github.com/cloudway/platform/container/conf"
)

func main() {
    err := conf.Initialize()
    if err != nil {
        fmt.Fprintln(os.Stderr, err)
        os.Exit(1)
    }

    if cmds.RootCommand.Execute() != nil {
        os.Exit(1)
    }
}

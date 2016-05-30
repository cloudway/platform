package main

import (
    "fmt"
    "os"
    "icloudway.com/platform/cmd/cwman/cmds"
    "icloudway.com/platform/container/conf"
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

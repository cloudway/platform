package main

import (
    "os"
    "github.com/cloudway/platform/cmd/cwctl/cmds"
)

func main() {
    if cmds.RootCommand.Execute() != nil {
        os.Exit(1)
    }
}

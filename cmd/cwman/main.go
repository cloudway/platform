package main

import (
    "os"
    "icloudway.com/platform/cmd/cwman/cmds"
)

func main() {
    if cmds.RootCommand.Execute() != nil {
        os.Exit(-1)
    }
}

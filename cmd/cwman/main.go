package main

import (
    "fmt"
    "os"
    "sort"
    "github.com/cloudway/platform/cmd/cwman/cmds"
    "github.com/cloudway/platform/container/conf"
    "github.com/spf13/cobra"
)

type byCmdName []*cobra.Command

func (a byCmdName) Len() int           { return len(a) }
func (a byCmdName) Swap(i, j int)      { a[i], a[j] = a[j], a[i] }
func (a byCmdName) Less(i, j int) bool { return a[i].Name() < a[j].Name() }

func main() {
    err := conf.Initialize()
    if err != nil {
        fmt.Fprintln(os.Stderr, err)
        os.Exit(1)
    }

    sort.Sort(byCmdName(cmds.RootCommand.Commands()))
    if cmds.RootCommand.Execute() != nil {
        os.Exit(1)
    }
}

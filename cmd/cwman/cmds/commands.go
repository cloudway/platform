package cmds

import (
    "fmt"
    "os"
    "strings"
    "github.com/spf13/cobra"
    "icloudway.com/platform/container"
)

// RootCommand is the root of the command tree.
var RootCommand = &cobra.Command{
    Use:   "cwman",
    Short: "Cloudway application container management tool",
}

func init() {
    RootCommand.PersistentFlags().BoolVarP(&container.DEBUG, "debug", "d", false, "debugging")
}

func check(err error) {
    if err != nil {
        fmt.Fprintln(os.Stderr, err)
        os.Exit(1)
    }
}

func runContainerAction(id string, action func (*container.Container) error) {
    if strings.ContainsRune(id, '-') {
        // assume the key is 'name-namespace'
        nns := strings.SplitN(id, "-", 2)
        containers, err := container.Find(nns[0], nns[1])
        check(err)

        if len(containers) == 0 {
            check(fmt.Errorf("%s: Not found", id))
        }

        for _, c := range containers {
            check(action(c))
        }
    } else {
        // assume the key is an application id
        c, err := container.FromId(id)
        check(err)
        check(action(c))
    }
}
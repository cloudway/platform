package cmds

import (
    "fmt"
    "os"
    "errors"
    "strings"
    "github.com/spf13/cobra"
    "github.com/Sirupsen/logrus"
    "github.com/cloudway/platform/container"
)

// RootCommand is the root of the command tree.
var RootCommand = &cobra.Command{
    Use:   "cwman",
    Short: "Cloudway application container management tool",
}

func init() {
    RootCommand.PersistentFlags().BoolVar(&container.DEBUG, "debug", false, "debugging mode")
    RootCommand.PersistentPreRun = func (cmd *cobra.Command, args []string) {
        if container.DEBUG {
            logrus.SetLevel(logrus.DebugLevel)
        }
    }
}

func check(err error) {
    if err != nil {
        logrus.Fatal(err)
        os.Exit(1)
    }
}

func checkContainerArg(cmd *cobra.Command, args []string) error {
    if len(args) == 0 {
        return errors.New(cmd.Name() + ": you must provide the contaienr ID or name")
    }
    return nil
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
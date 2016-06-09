package cmds

import (
    "fmt"
    "os"
    "errors"
    "regexp"
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

var reNamePattern = regexp.MustCompile(`^((\*|[a-z][a-z_0-9]*)\.)?([a-z][a-z_0-9]*)-([a-z][a-z_0-9]*)$`)

func splitContainerName(name string) (string, string, string) {
    m := reNamePattern.FindStringSubmatch(name)
    if m != nil && len(m) != 0 {
        return m[2], m[3], m[4]
    } else {
        return "", "", ""
    }
}

func runContainerAction(id string, action func (*container.Container) error) {
    service, name, namespace := splitContainerName(id)
    if name != "" && namespace != "" {
        var containers []*container.Container
        var err error

        if service == "" {
            // assume the key is 'name-namespace'
            containers, err = container.FindApplications(name, namespace)
            check(err)
        } else if service == "*" {
            containers, err = container.FindAll(name, namespace)
            check(err)
        } else {
            // assume the key is 'service.name-namespace'
            containers, err = container.FindService(name, namespace, service)
            check(err)
        }

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
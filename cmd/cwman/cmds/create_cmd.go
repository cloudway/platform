package cmds

import (
    "errors"
    "strings"
    "regexp"
    "github.com/spf13/cobra"
    "github.com/cloudway/platform/container"
)

var (
    capacity string
    scale int = 1
)

var _NAME_PATTERN = regexp.MustCompile(`^([a-z][a-z_0-9]*)-([a-z][a-z_0-9]*)$`)

func init() {
    cmdCreate := &cobra.Command{
        Use:     "create name-namespace image",
        Short:   "Create a new application container",
        PreRunE: checkCreateArgs,
        Run:     runCreate,
    }

    cmdCreate.Flags().StringVarP(&capacity, "capacity", "c", "", "Application capacity (small,medium,large)")
    cmdCreate.Flags().IntVarP(&scale, "scale", "s", 1, "Application scaling")

    RootCommand.AddCommand(cmdCreate)
}

func checkCreateArgs(cmd *cobra.Command, args []string) error {
    if len(args) != 2 {
        return errors.New("you must provide name-namespace and base image")
    }

    if !_NAME_PATTERN.MatchString(args[0]) {
        return errors.New("The name and namespace arguments can only containes " +
                          "lower case letters, digits, or underscores. " +
                          "The name and namespace arguments must separated by " +
                          "a dash (-) character.")
    }

    return nil
}

func runCreate(cmd* cobra.Command, args []string) {
    nns := strings.SplitN(args[0], "-", 2)

    config := make(map[string]string)
    config["name"] = nns[0]
    config["namespace"] = nns[1]
    config["baseImage"] = args[1]

    if capacity != "" {
        config["capacity"] = capacity
    }
    if (scale != 0) {
        config["scale"] = string(scale)
    }

    _, err := container.Create(config)
    check(err)
}

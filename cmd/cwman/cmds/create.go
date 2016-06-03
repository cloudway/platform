package cmds

import (
    "errors"
    "strings"
    "regexp"
    "fmt"
    "github.com/spf13/cobra"
    "github.com/cloudway/platform/container"
)

var _NAME_PATTERN = regexp.MustCompile(`^([a-z][a-z_0-9]*)-([a-z][a-z_0-9]*)$`)

func init() {
    cmdCreate := &cobra.Command{
        Use:     "create NAME-NAMESPACE FRAMEWORK",
        Short:   "Create a new application container",
        PreRunE: checkCreateArgs,
        Run:     runCreate,
    }

    cmdCreate.Flags().StringP("capacity", "c", "", "Application capacity (small,medium,large)")
    cmdCreate.Flags().IntP("scale", "s", 1, "Application scaling")

    RootCommand.AddCommand(cmdCreate)
}

func checkCreateArgs(cmd *cobra.Command, args []string) error {
    if len(args) != 2 {
        return errors.New("you must provide name-namespace and framework plugin")
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
    name_namespace  := strings.SplitN(args[0], "-", 2)
    name, namespace := name_namespace[0], name_namespace[1]

    config := map[string]string{
        "name":         name,
        "namespace":    namespace,
        "source":       args[1],
    }

    if cmd.Flags().Changed("capacity") {
        config["capacity"], _ = cmd.Flags().GetString("capacity")
    }

    scale, _ := cmd.Flags().GetInt("scale")
    scale, err := getScaling(name, namespace, scale)
    check(err)

    for i := 0; i < scale; i++ {
        _, err := container.Create(config)
        check(err)
    }
}

func getScaling(name, namespace string, scale int) (int, error) {
    if scale <= 0 {
        return 0, fmt.Errorf("Invalid scaling value, it must be greater than 0")
    }

    containers, err := container.Find(name, namespace)
    if err != nil {
        return 0, err
    }

    n := len(containers)
    if (scale <= n) {
        return 0, fmt.Errorf("Application containers already reached maximum scaling value. " +
                             "(maximum scaling = %d, existing containers = %d", scale, n)
    }

    return scale - n, nil
}

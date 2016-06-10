package cmds

import (
    "errors"
    "github.com/spf13/cobra"
    "github.com/cloudway/platform/container"
)

func init() {
    cmdCreate := &cobra.Command{
        Use:     "create [SERVICE.]NAME-NAMESPACE PLUGIN",
        Short:   "Create a new application container",
        PreRunE: checkCreateArgs,
        Run:     runCreate,
    }

    cmdCreate.Flags().StringP("user", "u", "", "Specifiy the username")
    cmdCreate.Flags().StringP("capacity", "c", "", "Application capacity (small,medium,large)")
    cmdCreate.Flags().IntP("scale", "s", 1, "Application scaling")

    RootCommand.AddCommand(cmdCreate)
}

func checkCreateArgs(cmd *cobra.Command, args []string) error {
    if len(args) != 2 {
        return errors.New("Illegal number of arguments.")
    }

    service, name, namespace := splitContainerName(args[0])
    if name == "" || namespace == "" || service == "*" {
        return (errors.New(
            "The name and namespace arguments can only containes " +
            "lower case letters, digits, or underscores. " +
            "The name and namespace arguments must separated by " +
            "a dash (-) character."))
    }

    return nil
}

func runCreate(cmd* cobra.Command, args []string) {
    service, name, namespace := splitContainerName(args[0])
    config := container.CreateOptions{
        Name:           name,
        Namespace:      namespace,
        ServiceName:    service,
        PluginSource:   args[1],
    }

    config.User, _     = cmd.Flags().GetString("user")
    config.Capacity, _ = cmd.Flags().GetString("capacity")
    config.Scaling,  _ = cmd.Flags().GetInt("scale")

    _, err := container.Create(config)
    check(err)
}

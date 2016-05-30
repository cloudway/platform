package cmds

import (
    "fmt"
    "github.com/spf13/cobra"
    "icloudway.com/platform/container"
)

func init() {
    cmdList := &cobra.Command{
        Use:   "list",
        Short: "List all application containers",
        Run:   runList,
    }

    RootCommand.AddCommand(cmdList)
}

func runList(cmd *cobra.Command, args []string) {
    containers, err := container.All()
    check(err)

    fmt.Printf("%-12s    %-8s    %s\n", "CONTAINER ID", "STATUS", "DOMAIN NAME")
    for _, c := range containers {
        fmt.Printf("%-12s    %-8s    %s\n", c.ID[:12], c.State(), c.FQDN())
    }
}
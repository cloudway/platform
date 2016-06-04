package cmds

import (
    "github.com/spf13/cobra"
    "fmt"
)

func init() {
    cmdHello := &cobra.Command{
        Use:    "hello",
        Short:  "Print hello world",
        Run:    runHello,
    }
    RootCommand.AddCommand(cmdHello)
}

func runHello(cmd *cobra.Command, args []string) {
    fmt.Printf("Hello, world!\n")
}

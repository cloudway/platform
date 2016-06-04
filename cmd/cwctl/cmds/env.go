package cmds

import (
    "fmt"
    "os"
    "github.com/spf13/cobra"
    "github.com/cloudway/platform/sandbox"
    "encoding/json"
)

func init() {
    cmdEnv := &cobra.Command{
        Use:    "env",
        Short:  "Print application environments",
        Run:    runEnv,
    }

    cmdEnv.Flags().BoolP("human", "H", false, "Print in human readable mode")
    RootCommand.AddCommand(cmdEnv)
}

func runEnv(cmd *cobra.Command, args []string) {
    env := sandbox.NewApplication().Environ()
    if b, _ := cmd.Flags().GetBool("human"); b {
        for k, v := range env {
            fmt.Printf("%s=%q\n", k, v)
        }
    } else {
        enc := json.NewEncoder(os.Stdout)
        enc.Encode(env)
    }
}

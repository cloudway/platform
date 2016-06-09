package cmds

import (
    "os"
    "github.com/spf13/cobra"
    "github.com/cloudway/platform/sandbox"
)

func init() {
    cmdUpload := &cobra.Command{
        Use:    "upload",
        Short:  "Upload files into application container",
        Run:    runUploadCmd,
        Hidden: true,
    }
    RootCommand.AddCommand(cmdUpload)
}

func runUploadCmd(cmd *cobra.Command, args []string) {
    app := sandbox.NewApplication()
    check(app.Upload(os.Stdin))
}

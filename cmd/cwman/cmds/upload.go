package cmds

import (
    "github.com/spf13/cobra"
    "github.com/cloudway/platform/container"
)

func init() {
    cmdUpload := &cobra.Command{
        Use:     "upload CONTAINER PATH",
        Short:   "Upload application files",
        PreRunE: checkContainerArg,
        Run:     runUploadCmd,
    }

    cmdUpload.Flags().Bool("repo", false, "Upload files into repo directory")
    RootCommand.AddCommand(cmdUpload)
}

func runUploadCmd(cmd *cobra.Command, args []string) {
    runContainerAction(args[0], func (c *container.Container) error {
        var dir string
        if len(args) == 1 {
            dir = "."
        } else {
            dir = args[1]
        }
        return c.Upload(dir)
    })
}

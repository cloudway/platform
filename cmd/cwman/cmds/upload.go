package cmds

import (
    "os"
    "errors"
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

func checkUploadArgs(cmd *cobra.Command, args []string) error {
    if len(args) != 2 {
        return errors.New(cmd.Name() + ": Invalid number of arguments")
    } else {
        return nil
    }
}

func runUploadCmd(cmd *cobra.Command, args []string) {
    runContainerAction(args[0], func (c *container.Container) error {
        repoOnly, _ := cmd.Flags().GetBool("repo")

        var dir string
        if len(args) == 1 {
            dir = "."
        } else {
            dir = args[1]
        }
        if dir == "-" {
            return c.UploadArchive(os.Stdin)
        } else {
            return c.UploadFiles(dir, repoOnly)
        }
    })
}

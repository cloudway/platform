package cmds

import (
    "os"
    "github.com/spf13/cobra"
    "github.com/cloudway/platform/container"
)

func init() {
    cmdDownload := &cobra.Command{
        Use:     "download CONTAINER",
        Short:   "Download application files",
        PreRunE: checkContainerArg,
        Run:     runDownloadCmd,
    }

    cmdDownload.Flags().Bool("repo", false, "Includes repo directory")
    cmdDownload.Flags().Bool("data", false, "Includes data directory")
    cmdDownload.Flags().Bool("logs", false, "Includes logs directory")
    cmdDownload.Flags().Bool("all",  false, "Includes all directories")
    RootCommand.AddCommand(cmdDownload)
}

func runDownloadCmd(cmd *cobra.Command, args []string) {
    runContainerAction(args[0], func (c *container.Container) error {
        var includes container.Includes
        if b, _ := cmd.Flags().GetBool("all"); b {
            includes = container.IncludeAll
        }
        if b, _ := cmd.Flags().GetBool("repo"); b {
            includes |= container.IncludeRepo
        }
        if b, _ := cmd.Flags().GetBool("data"); b {
            includes |= container.IncludeData
        }
        if b, _ := cmd.Flags().GetBool("logs"); b {
            includes |= container.IncludeLogs
        }

        if includes == 0 {
            includes = container.IncludeRepo
        }

        var dir string
        if len(args) == 1 {
            dir = "."
        } else {
            dir = args[1];
        }
        if dir == "-" {
            return c.DownloadArchive(includes, os.Stdout)
        } else {
            return c.DownloadFiles(includes, dir)
        }
    })
}

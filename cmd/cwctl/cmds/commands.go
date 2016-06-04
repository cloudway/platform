package cmds

import (
    "github.com/spf13/cobra"
    "github.com/Sirupsen/logrus"
    "os"
)

var RootCommand = &cobra.Command{
    Use:    "cwctl",
    Short:  "Cloudway application management tool",
}

func check(err error) {
    if err != nil {
        logrus.Fatal(err)
        os.Exit(1)
    }
}

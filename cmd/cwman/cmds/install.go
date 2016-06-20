package cmds

import (
    "github.com/cloudway/platform/hub"
    "github.com/cloudway/platform/pkg/mflag"
)

func (cli *CWMan) CmdInstallPlugin(args ...string) error {
    cmd := cli.Subcmd("install", "PATH...")
    cmd.Require(mflag.Min, 1)
    cmd.ParseFlags(args, true)

    hub, err := hub.New()
    if err != nil {
        return err
    }

    for _, path := range cmd.Args() {
        if err = hub.InstallPlugin("", path); err != nil {
            return err
        }
    }

    return nil
}

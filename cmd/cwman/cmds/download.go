package cmds

import (
    "os"
    "golang.org/x/net/context"
    "github.com/cloudway/platform/pkg/mflag"
    "github.com/cloudway/platform/pkg/archive"
    "github.com/cloudway/platform/container"
)

func (cli *CWMan) CmdDownload(args ...string) error {
    cmd := cli.Subcmd("download", "CONTAINER [PATH]")
    cmd.Require(mflag.Min, 1)
    cmd.Require(mflag.Max, 2)
    cmd.ParseFlags(args, true)

    return cli.runContainerAction(cmd.Arg(0), func (c *container.Container) error {
        var dir string
        if cmd.NArg() == 1 {
            dir = c.Name + "-" + c.Namespace
        } else {
            dir = cmd.Arg(1)
        }
        return download(c, dir, c.RepoDir()+"/.")
    })
}

func download(c *container.Container, dst, src string) (err error) {
    r, _, err := c.CopyFromContainer(context.Background(), c.ID, src)
    if err != nil {
        return err
    }
    defer r.Close()
    if err = os.MkdirAll(dst, 0755); err != nil {
        return err
    }
    return archive.ExtractFiles(dst, r)
}

package cmds

import (
    "fmt"
    "io"
    "io/ioutil"
    "os"
    "strings"
    "archive/tar"
    "compress/gzip"
    "github.com/cloudway/platform/pkg/mflag"
    "github.com/cloudway/platform/pkg/archive"
    "github.com/cloudway/platform/container"
)

func (cli *CWMan) CmdUpload(args ...string) error {
    cmd := cli.Subcmd("upload", "CONTAINER [PATH]")
    cmd.Require(mflag.Min, 1)
    cmd.Require(mflag.Max, 2)
    cmd.ParseFlags(args, true)

    var containers []*container.Container
    service, name, namespace := splitContainerName(cmd.Arg(0))
    if name != "" && namespace != "" {
        if service != "" && service != "*" {
            return fmt.Errorf("Cannot upload files to service containers")
        }
        cs, err := container.FindApplications(name, namespace)
        if err != nil {
            return err
        }
        containers = cs
    } else {
        c, err := container.FromId(cmd.Arg(0))
        if err != nil {
            return err
        }
        containers = []*container.Container{c}
    }

    var path string
    if cmd.NArg() == 1 {
        path = "."
    } else {
        path = cmd.Arg(1)
    }

    stat, err := os.Stat(path);
    if err != nil {
        return err
    }
    if stat.IsDir() {
        tmpfile, err := makeArchive(path)
        if err != nil {
            return err
        }
        defer os.Remove(tmpfile)
        path = tmpfile
    }

    for _, c := range containers {
        if err := upload(c, path); err != nil {
            return err
        }
    }
    return nil
}

func makeArchive(path string) (string, error) {
    f, err := ioutil.TempFile("", "upload")
    if err != nil {
        return "", err
    }
    defer f.Close()

    tw := tar.NewWriter(f)
    err = archive.CopyFileTree(tw, "", path, false)
    tw.Close()
    return f.Name(), err
}

func upload(c *container.Container, src string) (err error) {
    f, err := os.Open(src)
    if err != nil {
        return err
    }
    defer f.Close()

    var tf io.Reader
    if strings.HasSuffix(src, ".gz") {
        if tf, err = gzip.NewReader(f); err != nil {
            return err
        }
    } else {
        tf = f
    }
    return c.Exec("", tf, os.Stdout, os.Stderr, "/usr/bin/cwctl", "upload")
}

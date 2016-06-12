package container

import (
    "io"
    "io/ioutil"
    "os"
    "strings"
    "archive/tar"
    "compress/gzip"
    "github.com/cloudway/platform/pkg/archive"
)

func (c *Container) Upload(src string) (err error) {
    var tf io.Reader
    stat, err := os.Stat(src)
    if err != nil {
        return err
    }

    if stat.IsDir() {
        f, err := ioutil.TempFile("", "upload")
        if err != nil {
            return err
        }
        defer func() {
            f.Close()
            os.Remove(f.Name())
        }()

        tw := tar.NewWriter(f)
        err = archive.CopyFileTree(tw, "", src, false)
        if err != nil {
            return err
        }

        tw.Close()
        if _, err = f.Seek(0, 0); err != nil {
            return err
        }
        tf = f
    } else {
        f, err := os.Open(src)
        if err != nil {
            return err
        }
        defer f.Close()

        if strings.HasSuffix(src, ".gz") {
            if tf, err = gzip.NewReader(f); err != nil {
                return err
            }
        } else {
            tf = f
        }
    }

    return c.Exec("", tf, os.Stdout, os.Stderr, "/usr/bin/cwctl", "upload")
}

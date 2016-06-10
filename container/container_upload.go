package container

import (
    "io"
    "io/ioutil"
    "os"
    "strings"
    "path/filepath"
    "archive/tar"
    "compress/gzip"
    "github.com/cloudway/platform/container/archive"
)

func (c *Container) UploadArchive(r io.Reader) error {
    return c.Exec("", r, os.Stdout, os.Stderr, "/usr/bin/cwctl", "upload")
}

func (c *Container) UploadFiles(src string, repoOnly bool) (err error) {
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
        if repoOnly {
            err = archive.CopyFileTree(tw, "repo", src, false)
            if err != nil {
                return err
            }
        } else {
            for _, dir := range []string{"repo", "data", "logs"} {
                path := filepath.Join(src, dir)
                if _, err := os.Stat(path); os.IsNotExist(err) {
                    continue
                }
                err = archive.CopyFileTree(tw, dir, path, false);
                if err != nil {
                    return err
                }
            }
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

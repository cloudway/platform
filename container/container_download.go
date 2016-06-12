package container

import (
    "io"
    "os"
    "archive/tar"
    "github.com/docker/distribution/context"
    "github.com/cloudway/platform/pkg/archive"
)

type Includes uint8

const (
    IncludeRepo Includes = 1 << iota
    IncludeData
    IncludeLogs

    IncludeAll = IncludeData | IncludeRepo | IncludeLogs
)

func (c *Container) DownloadArchive(includes Includes, w io.Writer) (err error) {
    tw := tar.NewWriter(w)

    if (includes & IncludeRepo) != 0 {
        err = downloadArchive(tw, c, "repo", c.RepoDir())
    }
    if err == nil && (includes & IncludeData) != 0 {
        err = downloadArchive(tw, c, "data", c.DataDir())
    }
    if err == nil && (includes & IncludeLogs) != 0 {
        err = downloadArchive(tw, c, "logs", c.LogDir())
    }

    return tw.Close()
}

func downloadArchive(tw *tar.Writer, c *Container, dir, path string) (err error) {
    r, _, err := c.CopyFromContainer(context.Background(), c.ID, path)
    if err != nil {
        return err
    }
    defer r.Close()

    tr := tar.NewReader(r)
    for {
        hdr, err := tr.Next()
        if err == io.EOF {
            break
        }
        if err != nil {
            return err
        }

        if err = tw.WriteHeader(hdr); err != nil {
            return err
        }
        if hdr.Typeflag == tar.TypeReg {
            if _, err = io.Copy(tw, tr); err != nil {
                return err
            }
        }
    }

    return nil
}

func (c *Container) DownloadFiles(includes Includes, dst string) (err error) {
    if (includes & IncludeRepo) != 0 {
        err = downloadFiles(c, c.RepoDir(), dst)
    }
    if err == nil && (includes & IncludeData) != 0 {
        err = downloadFiles(c, c.DataDir(), dst)
    }
    if err == nil && (includes & IncludeLogs) != 0 {
        err = downloadFiles(c, c.LogDir(), dst)
    }
    return nil
}

func downloadFiles(c *Container, path, dst string) (err error) {
    r, _, err := c.CopyFromContainer(context.Background(), c.ID, path)
    if err != nil {
        return err
    }
    defer r.Close()

    if err = os.MkdirAll(dst, 0755); err != nil {
        return err
    }

    return archive.ExtractFiles(dst, r)
}

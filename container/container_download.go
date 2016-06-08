package container

import (
    "io"
    "archive/tar"
    "github.com/docker/distribution/context"
)

type Includes uint8

const (
    IncludeRepo Includes = 1 << iota
    IncludeData
    IncludeLogs

    IncludeAll = IncludeData | IncludeRepo | IncludeLogs
)

func (c *Container) Download(includes Includes, w io.Writer) (err error) {
    tw := tar.NewWriter(w)

    if (includes & IncludeRepo) != 0 {
        err = download(tw, c, "repo", c.RepoDir())
    }
    if err == nil && (includes & IncludeData) != 0 {
        err = download(tw, c, "data", c.DataDir())
    }
    if err == nil && (includes & IncludeLogs) != 0 {
        err = download(tw, c, "logs", c.LogDir())
    }

    return tw.Close()
}

func download(tw *tar.Writer, c *Container, dir, path string) (err error) {
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

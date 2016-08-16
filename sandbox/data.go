package sandbox

import (
    "io"
    "archive/tar"
    "github.com/cloudway/platform/pkg/archive"
)

func (box *Sandbox) Dump(sink io.Writer) (err error) {
    err = box.Control("pre-dump", false, false)
    if err != nil {
        return err
    }

    tw := tar.NewWriter(sink)
    err = archive.CopyFileTree(tw, "", box.DataDir(), nil, false)
    if err != nil {
        return err
    }
    if err = tw.Close(); err != nil {
        return err
    }

    return box.Control("post-dump", false, false)
}

func (box *Sandbox) Restore(source io.Reader) (err error) {
    err = box.Control("pre-restore", false, false)
    if err != nil {
        return err
    }

    err = archive.ExtractFiles(box.DataDir(), source)
    if err != nil {
        return err
    }

    return box.Control("post-restore", false, false)
}

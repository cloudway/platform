package files

import (
    "os"
    "io"
    "path/filepath"
    "github.com/Sirupsen/logrus"
    "strings"
    "compress/gzip"
    "github.com/cloudway/platform/pkg/archive"
)

func CopyFiles(src, dst string) error {
    return filepath.Walk(src, func(path string, info os.FileInfo, err error) error {
        if err != nil {
            return err
        }

        target, err := filepath.Rel(src, path)
        if err != nil {
            logrus.WithError(err).Debug("Failed to get relative path")
            return nil
        }
        target = filepath.Join(dst, target)
        logrus.Debugf("Copying %s to %s", path, target)

        if info.IsDir() {
            os.MkdirAll(target, info.Mode())
            return nil
        }

        in, err := os.Open(path)
        if err != nil {
            return err
        }
        defer in.Close()

        out, err := os.Create(target)
        if err != nil {
            return err
        }
        defer out.Close()

        _, err = io.Copy(out, in)
        if err != nil {
            return err
        }

        os.Chmod(target, info.Mode())
        os.Chtimes(target, info.ModTime(), info.ModTime())
        return nil
    })
}

func MoveFiles(src, dst string) (err error) {
    defer os.RemoveAll(src)

    if _, err = os.Stat(src); err != nil {
        return err
    }
    if err = os.RemoveAll(dst); err != nil {
        return err
    }
    if os.Rename(src, dst) == nil {
        return nil
    }
    if err = os.MkdirAll(dst, 0755); err != nil {
        return err
    }
    return CopyFiles(src, dst)
}

func ExtractFiles(src, dst string) (err error) {
    srcf, err := os.Open(src)
    if err != nil {
        return err
    }
    defer srcf.Close()

    switch {
    case strings.HasSuffix(src, ".tar.gz") || strings.HasSuffix(src, ".tgz"):
        zr, zerr := gzip.NewReader(srcf)
        if zerr != nil {
            return zerr
        }
        return archive.ExtractFiles(dst, zr)
    default:
        return archive.ExtractFiles(dst, srcf)
    }
}

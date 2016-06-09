package sandbox

import (
    "fmt"
    "os"
    "io"
    "archive/tar"
    "path/filepath"
    "github.com/Sirupsen/logrus"
)

func (app *Application) ExtractFiles(target string, source io.Reader) error {
    tr := tar.NewReader(source)

    for {
        hdr, err := tr.Next()
        if err != nil {
            if err == io.EOF {
                return nil
            }
            return err
        }

        dst := filepath.Join(target, hdr.Name)
        switch hdr.Typeflag {
        case tar.TypeDir:
            logrus.Debugf("Creating directory: %s", dst)
            if err = os.MkdirAll(dst, os.FileMode(hdr.Mode)); err != nil {
                return err
            }
            os.Chtimes(dst, hdr.AccessTime, hdr.ChangeTime)

        case tar.TypeReg:
            logrus.Debugf("Extracting %s", dst)
            os.MkdirAll(filepath.Dir(dst), 0755)
            w, err := os.Create(dst)
            if err != nil {
                return err
            }
            _, err = io.Copy(w, tr);
            w.Close()
            if err != nil {
                return err
            }
            os.Chmod(dst, os.FileMode(hdr.Mode));
            os.Chtimes(dst, hdr.AccessTime, hdr.ChangeTime)

        default:
            return fmt.Errorf("Unable to extract file %s", hdr.Name) // FIXME
        }
    }
}

func (app *Application) CopyFiles(dst, src string) error {
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

func (app *Application) MoveFiles(dst, src string) (err error) {
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
    return app.CopyFiles(dst, src)
}

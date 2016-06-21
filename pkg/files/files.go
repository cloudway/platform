package files

import (
    "os"
    "io"
    "sync"
    "time"
    "strconv"
    "strings"
    "path/filepath"
    "compress/gzip"
    "github.com/Sirupsen/logrus"
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

// Random number state.
var rand uint32
var randmu sync.Mutex

func reseed() uint32 {
    return uint32(time.Now().UnixNano() + int64(os.Getpid()))
}

func nextName() string {
    randmu.Lock()
    r := rand
    if r == 0 {
        r = reseed()
    }
    r = r*1664525 + 1013904223
    rand = r
    randmu.Unlock()
    return strconv.Itoa(int(1e9 + r%1e9))[1:]
}

func TempFile(dir, prefix, suffix string) (f *os.File, err error) {
    if dir == "" {
        dir = os.TempDir()
    }

    nconflict := 0
    for i := 0; i < 10000; i++ {
        name := filepath.Join(dir, prefix+nextName()+suffix)
        f, err = os.OpenFile(name, os.O_RDWR|os.O_CREATE|os.O_EXCL, 0600)
        if os.IsExist(err) {
            if nconflict++; nconflict > 10 {
                randmu.Lock()
                rand = reseed()
                randmu.Unlock()
            }
            continue
        }
        break
    }
    return
}

package archive

import (
    "fmt"
    "os"
    "io"
    "io/ioutil"
    "strings"
    "compress/gzip"
    "archive/tar"
    "path/filepath"
    "github.com/Sirupsen/logrus"
    "github.com/cloudway/platform/pkg/manifest"
)

type UnsupportedArchiveError struct {
    file string
}

func (e UnsupportedArchiveError) Error() string {
    return fmt.Sprintf("Unsupported plugin archive file: %s", e.file)
}

type InvalidArchiveError struct {
    file string
}

func (e InvalidArchiveError) Error() string {
    return fmt.Sprintf("Invalid plugin archive file: %s", e.file)
}

func ReadFile(path, name string) ([]byte, error) {
    stat, err := os.Stat(path)
    if err != nil {
        return nil, err
    }

    if stat.IsDir() {
        return ioutil.ReadFile(filepath.Join(path, filepath.FromSlash(name)))
    } else {
        f, err := os.Open(path)
        if err != nil {
            return nil, err
        }
        defer f.Close()

        r, err := openArchiveFile(f, name)
        if err != nil {
            return nil, err
        }
        return ioutil.ReadAll(r)
    }
}

func ReadManifest(path string) (*manifest.Plugin, error) {
    stat, err := os.Stat(path)
    if err != nil {
        return nil, err
    }

    var p *manifest.Plugin
    if stat.IsDir() {
        p, err = manifest.Load(path)
    } else {
        f, err := os.Open(path)
        if err != nil {
            return nil, err
        }
        defer f.Close()

        r, err := openArchiveFile(f, manifest.ManifestEntry)
        if err != nil {
            return nil, err
        }
        p, err = manifest.Read(r)
    }

    if err == nil {
        p.Path = path
    }
    return p, err
}

func openArchiveFile(f *os.File, name string) (io.Reader, error) {
    var path = f.Name()

    switch {
    case strings.HasSuffix(path, ".tar.gz") || strings.HasSuffix(path, ".tgz"):
        return openTarGzFile(f, name)
    case strings.HasSuffix(path, ".tar"):
        return openTarFile(path, f, name)
    default:
        return nil, UnsupportedArchiveError{file: path}
    }
}

func openTarGzFile(ar *os.File, name string) (io.Reader, error) {
    r, err := gzip.NewReader(ar)
    if err != nil {
        return nil, err
    }
    return openTarFile(ar.Name(), r, name)
}

func openTarFile(path string, r io.Reader, name string) (io.Reader, error) {
    tr := tar.NewReader(r)
    for {
        hdr, err := tr.Next()
        if err == io.EOF {
            break
        }
        if err != nil {
            return nil, err
        }
        if hdr.Name == name {
            return tr, nil
        }
    }
    return nil, InvalidArchiveError{file: path}
}

func AddFile(tw *tar.Writer, filename string, filemode int64, content []byte) error {
    hdr := &tar.Header {
        Name: filename,
        Mode: filemode,
        Size: int64(len(content)),
    }

    err := tw.WriteHeader(hdr)
    if err == nil {
        _, err = tw.Write(content)
    }
    return err
}

func CopyFile(tw *tar.Writer, path, filename string, filemode int64) error {
    f, err := os.Open(path)
    if err != nil {
        return err
    }
    defer f.Close()

    stat, err := f.Stat()
    if err != nil {
        return err
    }

    if filemode == 0 {
        filemode = int64(stat.Mode())
    }

    hdr := &tar.Header{
        Name: filename,
        Mode: filemode,
        Size: stat.Size(),
    }

    if err = tw.WriteHeader(hdr); err == nil {
        _, err = io.Copy(tw, f)
    }
    return err
}

func CopyFileTree(tw* tar.Writer, dst, src string, followLinks bool) error {
    return filepath.Walk(src, func (path string, info os.FileInfo, err error) error {
        if err != nil || info.IsDir() {
            return err
        }

        relpath, err := filepath.Rel(src, path)
        if err != nil {
            logrus.WithError(err).Debug("Failed to get relative path")
            return nil
        }
        if len(relpath) == 0 {
            return nil
        }
        if len(dst) != 0 {
            relpath = filepath.ToSlash(filepath.Join(dst, relpath))
        }
        logrus.Debugf("Copying %s to %s", path, relpath)

        fr, err := os.Open(path)
        if err != nil {
            return err
        }
        defer fr.Close()

        if followLinks && (info.Mode() & os.ModeSymlink) != 0 {
            if info, err = os.Stat(path); err != nil {
                return err
            }
        }

        if hdr, err := tar.FileInfoHeader(info, relpath); err != nil {
            return err
        } else {
            hdr.Name = relpath
            if err = tw.WriteHeader(hdr); err != nil {
                return err
            }
        }

        _, err = io.Copy(tw, fr)
        return err
    })
}

func ExtractFiles(dst string, r io.Reader) error {
    tr := tar.NewReader(r)

    for {
        hdr, err := tr.Next()
        if err != nil {
            if err == io.EOF {
                return nil
            }
            return err
        }

        dst := filepath.Join(dst, hdr.Name)
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

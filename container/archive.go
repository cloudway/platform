package container

import (
    "fmt"
    "os"
    "io"
    "io/ioutil"
    "strings"
    "archive/zip"
    "compress/gzip"
    "archive/tar"
    "path/filepath"
    "github.com/cloudway/platform/plugin"
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

func readFile(path, name string) ([]byte, error) {
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

func readManifest(path string) (*plugin.Plugin, error) {
    stat, err := os.Stat(path)
    if err != nil {
        return nil, err
    }

    var p *plugin.Plugin
    if stat.IsDir() {
        p, err = plugin.Load(path, nil)
    } else {
        f, err := os.Open(path)
        if err != nil {
            return nil, err
        }
        defer f.Close()

        r, err := openArchiveFile(f, plugin.ManifestEntry)
        if err != nil {
            return nil, err
        }
        p, err = plugin.ReadManifest(r)
    }

    if err == nil && (!p.IsFramework() || p.BaseImage == "") {
        return nil, fmt.Errorf("%s is not a valid framework plugin", path)
    } else {
        p.Path = path
        return p, err
    }
}

func openArchiveFile(f *os.File, name string) (io.Reader, error) {
    path := f.Name()
    switch {
    case strings.HasSuffix(path, ".zip"):
        return openZipFile(f, name)
    case strings.HasSuffix(path, ".tar.gz") || strings.HasSuffix(path, ".tgz"):
        return openTarGzFile(f, name)
    case strings.HasSuffix(path, ".tar"):
        return openTarFile(path, f, name)
    default:
        return nil, UnsupportedArchiveError{file: path}
    }
}

func openZipFile(ar *os.File, name string) (io.Reader, error) {
    fi, err := ar.Stat()
    if err != nil {
        return nil, err
    }
    r, err := zip.NewReader(ar, fi.Size())
    if err != nil {
        return nil, err
    }
    for _, f := range r.File {
        if f.Name == name {
            return f.Open()
        }
    }
    return nil, InvalidArchiveError{file: ar.Name()}
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

func addFile(tw *tar.Writer, filename string, filemode int64, content []byte) error {
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

func copyFile(tw* tar.Writer, filename string, filemode, filesize int64, r io.Reader) error {
    hdr := &tar.Header{
        Name: filename,
        Mode: filemode,
        Size: filesize,
    }

    err := tw.WriteHeader(hdr)
    if err == nil {
        _, err = io.Copy(tw, r)
    }
    return err
}

func copyFileTree(tw* tar.Writer, src, dst string) error {
    return filepath.Walk(src, func (path string, info os.FileInfo, err error) error {
        if err != nil || info.IsDir() {
            return err
        }

        relpath := path[len(src):]
        if len(relpath) == 0 {
            return nil
        }
        relpath = dst + "/" + relpath
        fr, err := os.Open(path)
        if err != nil {
            return err
        }
        defer fr.Close()

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

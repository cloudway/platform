package container

import (
    "fmt"
    "os"
    "io"
    "strings"
    "archive/zip"
    "compress/gzip"
    "archive/tar"
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

func readManifest(path string) (*plugin.Plugin, error) {
    stat, err := os.Stat(path)
    if err != nil {
        return nil, err
    }

    var p *plugin.Plugin
    if stat.IsDir() {
        p, err = plugin.Load(path, nil)
    } else {
        p, err = readManifestFromArchive(path)
    }

    if err == nil && (!p.IsFramework() || p.BaseImage == "") {
        return nil, fmt.Errorf("%s is not a valid framework plugin", path)
    } else {
        p.Path = path
        return p, err
    }
}

func readManifestFromArchive(path string) (*plugin.Plugin, error) {
    f, err := os.Open(path)
    if err != nil {
        return nil, err
    }
    defer f.Close()

    switch {
    case strings.HasSuffix(path, ".zip"):
        return readZipFile(f)
    case strings.HasSuffix(path, ".tar.gz"), strings.HasSuffix(path, ".tgz"):
        return readTarGzFile(f)
    case strings.HasSuffix(path, ".tar"):
        return readTarFile(f)
    default:
        return nil, UnsupportedArchiveError{file: path}
    }
}

func readZipFile(source *os.File) (*plugin.Plugin, error) {
    fi, err := source.Stat()
    if err != nil {
        return nil, err
    }
    r, err := zip.NewReader(source, fi.Size())
    if err != nil {
        return nil, err
    }

    for _, f := range r.File {
        if f.Name == plugin.ManifestEntry {
            rc, err := f.Open()
            if err != nil {
                return nil, err
            }
            return plugin.ReadManifest(rc)
        }
    }

    return nil, InvalidArchiveError{file: source.Name()}
}

func readTarGzFile(source *os.File) (*plugin.Plugin, error) {
    r, err := gzip.NewReader(source)
    if err != nil {
        return nil, err
    }
    return readTar(source.Name(), r)
}

func readTarFile(source *os.File) (*plugin.Plugin, error) {
    return readTar(source.Name(), source)
}

func readTar(path string, r io.Reader) (*plugin.Plugin, error) {
    tr := tar.NewReader(r)
    for {
        hdr, err := tr.Next()
        if err == io.EOF {
            break
        }
        if err != nil {
            return nil, err
        }
        if hdr.Name == plugin.ManifestEntry {
            return plugin.ReadManifest(tr)
        }
    }
    return nil, InvalidArchiveError{file: path}
}

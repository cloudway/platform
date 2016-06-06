package container

import (
    "errors"
    "os"
    "io/ioutil"
    "archive/tar"
    "github.com/cloudway/platform/container/archive"
    "strings"
    "compress/gzip"
    "io"
    "github.com/Sirupsen/logrus"
)

func (c *Container) Install(source string) error {
    if !c.State.Running || c.State.Paused {
        return errors.New("Container is not running")
    }

    // read plugin manifest
    meta, err := archive.ReadManifest(source)
    if err != nil {
        return err
    }
    if meta.IsFramework() {
        return errors.New(meta.Name+": Cannnot be a framework plugin")
    }

    // create plugin archive file
    var tarFile io.Reader
    stat, err := os.Stat(source)
    if err != nil {
        return err
    }
    if stat.IsDir() {
        f, err := ioutil.TempFile("", "docker")
        if err != nil {
            return err
        }
        defer func() {
            f.Close()
            os.Remove(f.Name())
        }()

        tw := tar.NewWriter(f)
        if err = archive.CopyFileTree(tw, "", source, false); err != nil {
            return err
        }
        tw.Close()
        if _, err = f.Seek(0, 0); err != nil {
            return err
        }

        tarFile = f
        logrus.Debugf("Created temporary archive file: %s", f.Name())
    } else {
        f, err := os.Open(source)
        if err != nil {
            return err
        }
        defer f.Close()

        if strings.HasSuffix(source, ".gz") {
            if tarFile, err = gzip.NewReader(f); err != nil {
                return err
            }
        } else {
            tarFile = f
        }
    }

    // run install command in application container
    return c.Exec("root", tarFile, os.Stdout, os.Stderr, "/usr/bin/cwctl", "install", meta.Name)
}

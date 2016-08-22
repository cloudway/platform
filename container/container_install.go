package container

import (
	"archive/tar"
	"compress/gzip"
	"errors"
	"fmt"
	"io"
	"io/ioutil"
	"os"
	"strings"

	"github.com/Sirupsen/logrus"
	"github.com/cloudway/platform/pkg/archive"
	"golang.org/x/net/context"
)

func (c *Container) Install(ctx context.Context, source string) error {
	if !c.State.Running || c.State.Paused {
		return errors.New("Container is not running")
	}

	// read plugin manifest
	meta, err := archive.ReadManifest(source)
	if err != nil {
		return err
	}
	if !meta.IsLibrary() {
		return fmt.Errorf("%s: Must be a library plugin", meta.Name)
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
		if err = archive.CopyFileTree(tw, "", source, nil, false); err != nil {
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
	return c.Exec(ctx, "root", tarFile, os.Stdout, os.Stderr, "/usr/bin/cwctl", "install", meta.Name)
}

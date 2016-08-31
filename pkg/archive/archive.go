package archive

import (
	"archive/tar"
	"compress/gzip"
	"fmt"
	"io"
	"io/ioutil"
	"os"
	"path/filepath"
	"strings"

	"github.com/Sirupsen/logrus"
	"github.com/cloudway/platform/pkg/manifest"
)

type InvalidArchiveError string

func (e InvalidArchiveError) Error() string {
	return fmt.Sprintf("Invalid plugin archive file: %s", string(e))
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
	default:
		return openTarFile(path, f, name)
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
	return nil, InvalidArchiveError(path)
}

func AddFile(tw *tar.Writer, filename string, filemode int64, content []byte) error {
	hdr := &tar.Header{
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

func CopyFileTree(tw *tar.Writer, dst, src string, excludes []string, followLinks bool) error {
	fi, err := os.Lstat(src)
	if err != nil {
		return err
	}
	if fi.Mode()&os.ModeSymlink != 0 {
		if link, err := os.Readlink(src); err != nil {
			return err
		} else {
			src = link
		}
	}

	return filepath.Walk(src, func(path string, info os.FileInfo, err error) error {
		if err != nil {
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
		for _, exc := range excludes {
			if relpath == exc || strings.HasPrefix(filepath.ToSlash(relpath), exc+"/") {
				if info.IsDir() {
					logrus.Debugf("Excluded directory %s", relpath)
					return filepath.SkipDir
				} else {
					logrus.Debugf("Excluded file %s", relpath)
					return nil
				}
			}
		}
		if info.IsDir() {
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

		if followLinks && (info.Mode()&os.ModeSymlink) != 0 {
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

func ExtractFiles(extractDir string, r io.Reader) error {
	tr := tar.NewReader(r)

	for {
		hdr, err := tr.Next()
		if err != nil {
			if err == io.EOF {
				return nil
			}
			return err
		}

		hdrInfo := hdr.FileInfo()
		dst := filepath.Join(extractDir, hdr.Name)

		switch hdr.Typeflag {
		case tar.TypeDir:
			// Create directory unless it exists as a directory already.
			// In that case we just want to merge the two.
			logrus.Debugf("Creating directory: %s", dst)
			if fi, err := os.Lstat(dst); !(err == nil && fi.IsDir()) {
				if err := os.Mkdir(dst, hdrInfo.Mode()); err != nil {
					return err
				}
			}

		case tar.TypeReg, tar.TypeRegA:
			logrus.Debugf("Extracting %s", dst)
			os.MkdirAll(filepath.Dir(dst), 0755)
			w, err := os.OpenFile(dst, os.O_CREATE|os.O_TRUNC|os.O_WRONLY, hdrInfo.Mode())
			if err != nil {
				return err
			}
			if _, err := io.Copy(w, tr); err != nil {
				w.Close()
				return err
			}
			w.Close()

		case tar.TypeLink:
			targetPath := filepath.Join(extractDir, hdr.Linkname)
			// check for hardlink breakout
			if !strings.HasPrefix(targetPath, extractDir) {
				return fmt.Errorf("invalid hardlink %q -> %q", dst, hdr.Linkname)
			}
			os.MkdirAll(filepath.Dir(dst), 0755)
			if err := os.Link(targetPath, dst); err != nil {
				return err
			}

		case tar.TypeSymlink:
			targetPath := filepath.Join(filepath.Dir(dst), hdr.Linkname)
			if !strings.HasPrefix(targetPath, extractDir) {
				return fmt.Errorf("invalid symlink %q -> %q", dst, hdr.Linkname)
			}
			os.MkdirAll(filepath.Dir(dst), 0755)
			if err := os.Symlink(hdr.Linkname, dst); err != nil {
				return err
			}

		case tar.TypeBlock, tar.TypeChar, tar.TypeFifo:
			logrus.Debugf("Ignored device file: %s", hdr.Name)

		case tar.TypeXGlobalHeader:
			logrus.Debugf("PAX Global Extended Headers found and ignored")

		default:
			return fmt.Errorf("Unable to extract file %s", hdr.Name)
		}
	}
}

func PrepareRepo(content io.Reader, zip bool) (repodir string, err error) {
	// create a temporary directory to hold deployment archive
	repodir, err = ioutil.TempDir("", "deploy")
	if err != nil {
		return "", err
	}

	// save archive to a temporary file
	filename := filepath.Join(repodir, filepath.Base(repodir)+".tar.gz")
	repofile, err := os.Create(filename)
	if err != nil {
		return
	}
	defer repofile.Close()

	if zip {
		w := gzip.NewWriter(repofile)
		_, err = io.Copy(w, content)
		if err == nil {
			err = w.Close()
		}
	} else {
		_, err = io.Copy(repofile, content)
	}
	return
}

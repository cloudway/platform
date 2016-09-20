package sandbox

import (
	"fmt"
	"io"
	"os"
	"path/filepath"
	"strings"

	"github.com/Sirupsen/logrus"

	"github.com/cloudway/platform/pkg/archive"
	"github.com/cloudway/platform/pkg/files"
	"github.com/cloudway/platform/pkg/manifest"
)

// Install plugin in the application. The plugin directory should already
// been populated from broker.
func (box *Sandbox) Install(name string, source string, input io.Reader) error {
	if os.Getuid() != 0 {
		return os.ErrPermission
	}

	var err error

	// create plugin directory
	target := filepath.Join(box.HomeDir(), name)
	if _, err = os.Stat(target); err == nil {
		return fmt.Errorf("Plugin already installed: %s", name)
	}
	if err = os.Mkdir(target, 0755); err != nil {
		return err
	}

	if input != nil {
		err = archive.ExtractFiles(target, input)
	} else {
		err = files.MoveFiles(source, target)
	}

	if err == nil {
		err = box.installPlugin(target)
	}

	if err != nil {
		// remove plugin directory if error occurred
		os.RemoveAll(target)
	}
	return err
}

func (box *Sandbox) installPlugin(target string) error {
	// load plugin manifest from target directory
	meta, err := manifest.Load(target)
	if err != nil {
		return err
	}
	name := meta.Name

	// add environemnt variables
	box.Setenv("CLOUDWAY_"+strings.ToUpper(name)+"_DIR", target, false)
	box.Setenv("CLOUDWAY_"+strings.ToUpper(name)+"_VERSION", meta.Version, false)
	if meta.IsFramework() {
		box.Setenv("CLOUDWAY_FRAMEWORK", name, false)
		box.Setenv("CLOUDWAY_FRAMEWORK_DIR", target, false)
	}

	// create log dir
	logdir := filepath.Join(box.LogDir(), name)
	os.MkdirAll(logdir, 0750)
	os.Chown(logdir, box.uid, box.gid)
	box.Setenv("CLOUDWAY_"+strings.ToUpper(name)+"_LOG_DIR", logdir, false)

	// create cache directories
	for _, cache := range meta.BuildCache {
		cachedir := filepath.Join(box.HomeDir(), cache)
		os.MkdirAll(cachedir, 0750)
		os.Chown(cachedir, box.uid, box.gid)
	}

	// run install script for non-framework plugin
	if meta.IsLibrary() {
		if err = runPluginAction(target, target, nil, "install"); err != nil {
			logrus.WithError(err).Error("run install script failed")
			return err
		}
	}

	// run setup script to setup the plugin
	if err = runPluginAction(target, target, MakeExecEnv(box.Environ()), "setup"); err != nil {
		logrus.WithError(err).Error("run setup script failed")
		return err
	}

	// remove unused setup scripts
	os.Remove(filepath.Join(target, "bin", "install"))
	os.Remove(filepath.Join(target, "bin", "setup"))

	// change owner of plugin directory
	chownR(target, box.uid, box.gid)
	chownR(filepath.Join(target, "manifest"), 0, box.gid)

	return nil
}

func chownR(root string, uid, gid int) error {
	return filepath.Walk(root, func(path string, info os.FileInfo, err error) error {
		if err == nil {
			err = os.Lchown(path, uid, gid)
		}
		return err
	})
}

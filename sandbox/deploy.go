package sandbox

import (
	"compress/gzip"
	"io/ioutil"
	"os"
	"path/filepath"
	"strings"

	"github.com/cloudway/platform/pkg/archive"
	"github.com/cloudway/platform/pkg/files"
)

func (box *Sandbox) Build() (err error) {
	primary, err := box.PrimaryPlugin()
	if err != nil {
		return err
	}
	if err = processTemplates(primary.Path, box.Environ()); err != nil {
		return err
	}
	return runPluginAction(primary.Path, box.RepoDir(), makeExecEnv(box.Environ()), "build")
}

func (box *Sandbox) Deploy() error {
	base := box.DeployDir()

	deployments, err := deployments(base)
	if err != nil {
		return err
	}
	if len(deployments) == 0 {
		return nil
	}
	defer removeDeployments(base, deployments)

	latest := latestDeployment(deployments)
	err = box.checkout(latest.Name())
	if err != nil {
		return err
	}

	primary, err := box.PrimaryPlugin()
	if err != nil {
		return err
	}
	err = processTemplates(primary.Path, box.Environ())
	if err != nil {
		return err
	}
	return runPluginAction(primary.Path, box.RepoDir(), makeExecEnv(box.Environ()), "deploy")
}

func (box *Sandbox) hasDeployments() bool {
	deployments, _ := deployments(box.DeployDir())
	return len(deployments) != 0
}

func deployments(deployDir string) ([]os.FileInfo, error) {
	f, err := os.Open(deployDir)
	if err != nil {
		return nil, err
	}
	files, err := f.Readdir(0)
	if err != nil {
		return nil, err
	}

	var deployments []os.FileInfo
	for _, fi := range files {
		if strings.HasPrefix(fi.Name(), "deploy") && strings.HasSuffix(fi.Name(), ".tar.gz") {
			deployments = append(deployments, fi)
		}
	}
	return deployments, nil
}

func removeDeployments(deployDir string, deployments []os.FileInfo) {
	for _, d := range deployments {
		os.Remove(filepath.Join(deployDir, d.Name()))
	}
}

func latestDeployment(deployments []os.FileInfo) os.FileInfo {
	var last *os.FileInfo
	for _, d := range deployments {
		if last == nil {
			last = &d
		} else if d.ModTime().After((*last).ModTime()) {
			last = &d
		}
	}
	return *last
}

func (box *Sandbox) checkout(name string) (err error) {
	f, err := os.Open(filepath.Join(box.DeployDir(), name))
	if err != nil {
		return err
	}
	defer f.Close()

	zr, err := gzip.NewReader(f)
	if err != nil {
		return err
	}

	// Create temporary directory to extract deployment
	tmpdir, err := ioutil.TempDir("", "deploy")
	if err != nil {
		return err
	}
	defer os.RemoveAll(tmpdir)

	// extract archive file into temporary directory
	if err = archive.ExtractFiles(tmpdir, zr); err != nil {
		return err
	}

	// move files into destination
	err = files.MoveFiles(tmpdir, box.RepoDir())
	if err != nil && !os.IsNotExist(err) {
		return err
	}

	return nil
}

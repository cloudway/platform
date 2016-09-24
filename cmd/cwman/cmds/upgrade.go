package cmds

import (
	"archive/tar"
	"context"
	"io/ioutil"
	"os"
	"path/filepath"

	"github.com/Sirupsen/logrus"

	"github.com/cloudway/platform/config"
	"github.com/cloudway/platform/container"
	"github.com/cloudway/platform/pkg/archive"
	"github.com/cloudway/platform/pkg/mflag"
	"github.com/cloudway/platform/pkg/serverlog"
	"github.com/docker/engine-api/types"
)

func (cli *CWMan) CmdUpgrade(args ...string) error {
	cmd := cli.Subcmd("upgrade", "")
	cmd.Require(mflag.Exact, 0)
	cmd.ParseFlags(args, true)

	// create an archive file that contains the support files
	ar, err := makeSupportArchive()
	if err != nil {
		return err
	}
	defer os.Remove(ar)

	ctx := context.Background()

	containers, err := cli.FindInNamespace(ctx, "")
	if err != nil {
		return err
	}

	// group containers by applications
	apps := make(map[string][]*container.Container)
	for _, c := range containers {
		key := c.Name + "-" + c.Namespace
		apps[key] = append(apps[key], c)
	}

	// Stop applications, copy support files, and restart applications
	cpopts := types.CopyToContainerOptions{}
	for app, cs := range apps {
		logrus.Infof("upgrading application %s", app)
		logError(container.ResolveServiceDependencies(cs))
		for _, c := range cs {
			file, err := os.Open(ar)
			if err != nil {
				return err
			}
			logrus.Infof("upgrading container %s.%s-%s", c.ServiceName(), c.Name, c.Namespace)
			logError(c.Stop(ctx))
			logError(c.CopyToContainer(ctx, c.ID, "/", file, cpopts))
			logError(c.Start(ctx, serverlog.Encap(os.Stdout, os.Stderr)))
			file.Close()
		}
	}

	return nil
}

func makeSupportArchive() (string, error) {
	file, err := ioutil.TempFile("", "tmp")
	if err != nil {
		return "", err
	}

	tw := tar.NewWriter(file)
	src := filepath.Join(config.RootDir, "sandbox")
	err = archive.CopyFileTree(tw, "", src, nil, false)

	if err != nil {
		file.Close()
		os.Remove(file.Name())
		return "", err
	} else {
		tw.Close()
		file.Close()
		return file.Name(), nil
	}
}

func logError(err error) {
	if err != nil {
		logrus.Error(err)
	}
}

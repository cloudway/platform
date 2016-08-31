package scm

import (
	"compress/gzip"
	"fmt"
	"io"
	"io/ioutil"
	"math/rand"
	"os"
	"path/filepath"

	"github.com/docker/engine-api/types"
	"golang.org/x/net/context"

	"github.com/cloudway/platform/container"
	"github.com/cloudway/platform/hub"
	"github.com/cloudway/platform/pkg/manifest"
)

// DeployRepository is a helper function used by SCM implementations
// to deploy an application repository.
func DeployRepository(cli container.DockerClient, ctx context.Context, name, namespace string, ids []string, in io.Reader, stdout, stderr io.Writer) error {
	containers, err := cli.FindApplications(ctx, name, namespace)
	if err != nil {
		return err
	}
	if len(containers) == 0 {
		return fmt.Errorf("%s: application not found", name)
	}

	// randomly select a base container
	var base *container.Container
	if len(containers) == 1 {
		base = containers[0]
	} else {
		base = containers[rand.Intn(len(containers))]
	}

	// save or build repository archive
	var repodir string
	if base.Flags()&container.HotDeployable != 0 {
		repodir, err = save(in, false)
	} else {
		repodir, err = build(cli, ctx, base, in, stdout, stderr)
	}

	if repodir != "" {
		defer os.RemoveAll(repodir)
	}
	if err != nil {
		return err
	}

	// distribute the repository archive
	return distribute(cli, ctx, containers, ids, repodir)
}

func build(cli container.DockerClient, ctx context.Context, base *container.Container, in io.Reader, stdout, stderr io.Writer) (repodir string, err error) {
	// make a fake plugin (you can make a real plugin from base container if you want)
	_, _, pn, pv, _ := hub.ParseTag(base.PluginTag())
	plugin := &manifest.Plugin{
		Tag:      base.PluginTag(),
		Name:     pn,
		Version:  pv,
		Category: base.Category(),
	}

	// create a builder container
	opts := container.CreateOptions{
		Name:      base.Name,
		Namespace: base.Namespace,
		Plugin:    plugin,
		Image:     base.Config.Image,
		Home:      base.Home(),
		User:      base.User(),
	}
	builder, err := cli.CreateBuilder(ctx, opts)
	if err != nil {
		return
	}
	defer builder.Destroy(ctx)

	// start builder container and build the application
	err = builder.ContainerStart(ctx, builder.ID, types.ContainerStartOptions{})
	if err != nil {
		return
	}
	err = builder.Exec(ctx, "", in, stdout, stderr, "/usr/bin/cwctl", "build")
	if err != nil {
		return
	}

	// download application repository from builder container
	r, _, err := builder.CopyFromContainer(ctx, builder.ID, builder.RepoDir()+"/.")
	if err != nil {
		return
	}
	defer r.Close()

	return save(r, true)
}

func save(r io.Reader, zip bool) (repodir string, err error) {
	repodir, err = ioutil.TempDir("", "deploy")
	if err != nil {
		return "", err
	}

	repofile, err := os.Create(filepath.Join(repodir, filepath.Base(repodir)+".tar.gz"))
	if err != nil {
		return
	}
	defer repofile.Close()

	if zip {
		w := gzip.NewWriter(repofile)
		_, err = io.Copy(w, r)
		if err == nil {
			err = w.Close()
		}
	} else {
		_, err = io.Copy(repofile, r)
	}
	return
}

func distribute(cli container.DockerClient, ctx context.Context, containers []*container.Container, ids []string, path string) (err error) {
	if len(ids) != 0 {
		containers = make([]*container.Container, len(ids))
		for i, id := range ids {
			containers[i], err = cli.Inspect(ctx, id)
			if err != nil {
				return err
			}
		}
	}

	for _, c := range containers {
		if c.Category().IsFramework() {
			er := c.Deploy(ctx, path)
			if er != nil {
				err = er
			}
		}
	}

	return err
}

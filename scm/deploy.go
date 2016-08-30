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
func DeployRepository(cli container.DockerClient, ctx context.Context, name, namespace string, ids []string, repo io.Reader) error {
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
		Name:      name,
		Namespace: namespace,
		Plugin:    plugin,
		Image:     base.Config.Image,
		Home:      base.Home(),
		User:      base.User(),
		Log:       os.Stdout,
	}
	builder, err := cli.CreateBuilder(ctx, opts)
	if err != nil {
		return err
	}
	defer builder.Destroy(ctx)

	// start builder container and build the application
	err = builder.ContainerStart(ctx, builder.ID, types.ContainerStartOptions{})
	if err != nil {
		return err
	}
	err = builder.Exec(ctx, "", repo, os.Stdout, os.Stderr, "/usr/bin/cwctl", "build")
	if err != nil {
		return err
	}

	// download application repository from builder container
	repodir, err := download(cli, ctx, builder)
	if err != nil {
		return err
	}
	defer os.RemoveAll(repodir)

	// distribute the repository archive
	return distribute(cli, ctx, containers, ids, repodir)
}

func download(cli container.DockerClient, ctx context.Context, builder *container.Container) (repodir string, err error) {
	repodir, err = ioutil.TempDir("", "deploy")
	if err != nil {
		return
	}

	repofile, err := os.Create(filepath.Join(repodir, filepath.Base(repodir)+".tar.gz"))
	if err != nil {
		return
	}
	defer repofile.Close()

	r, _, err := builder.CopyFromContainer(ctx, builder.ID, builder.RepoDir()+"/.")
	if err != nil {
		return
	}

	w := gzip.NewWriter(repofile)
	_, err = io.Copy(w, r)
	if err == nil {
		err = w.Close()
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
		er := c.Deploy(ctx, path)
		if er != nil {
			err = er
		}
	}

	return err
}

package scm

import (
	"archive/tar"
	"fmt"
	"io"
	"io/ioutil"
	"math/rand"
	"os"

	"github.com/docker/engine-api/types"
	"golang.org/x/net/context"
	"gopkg.in/yaml.v2"

	"github.com/cloudway/platform/container"
	"github.com/cloudway/platform/hub"
	"github.com/cloudway/platform/pkg/archive"
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
		repodir, err = archive.PrepareRepo(in, false)
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
	plugin, err := readPluginManifestFromContainer(ctx, base)
	if err != nil {
		return
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

	// start builder container
	err = builder.ContainerStart(ctx, builder.ID, types.ContainerStartOptions{})
	if err != nil {
		return
	}

	// build the application, use cache during build
	copyCache(ctx, plugin, base, builder, true)
	err = builder.Exec(ctx, "", in, stdout, stderr, "/usr/bin/cwctl", "build")
	if err != nil {
		return
	}
	copyCache(ctx, plugin, builder, base, false)

	// download application repository from builder container
	r, _, err := builder.CopyFromContainer(ctx, builder.ID, builder.RepoDir()+"/.")
	if err != nil {
		return
	}
	defer r.Close()

	return archive.PrepareRepo(r, true)
}

func readPluginManifestFromContainer(ctx context.Context, base *container.Container) (meta *manifest.Plugin, err error) {
	_, _, pn, _, _ := hub.ParseTag(base.PluginTag())
	path := fmt.Sprintf("%s/%s/manifest/plugin.yml", base.Home(), pn)
	r, _, err := base.CopyFromContainer(ctx, base.ID, path)
	if err != nil {
		return
	}
	defer r.Close()

	var content []byte
	tr := tar.NewReader(r)
	if _, err = tr.Next(); err != nil {
		return
	}
	if content, err = ioutil.ReadAll(tr); err != nil {
		return
	}

	var plugin manifest.Plugin
	err = yaml.Unmarshal(content, &plugin)
	if err != nil {
		return
	}

	plugin.Tag = base.PluginTag()
	return &plugin, err
}

func copyCache(ctx context.Context, plugin *manifest.Plugin, from, to *container.Container, chown bool) {
	if len(plugin.BuildCache) == 0 {
		return
	}

	var paths = make([]string, len(plugin.BuildCache))
	for i, cache := range plugin.BuildCache {
		paths[i] = from.Home() + "/" + cache
	}

	opts := types.CopyToContainerOptions{AllowOverwriteDirWithFile: true}
	for _, path := range paths {
		content, _, err := from.CopyFromContainer(ctx, from.ID, path+"/.")
		if err == nil {
			to.CopyToContainer(ctx, to.ID, path+"/", content, opts)
			content.Close()
		}
	}

	if chown {
		args := append([]string{"chown", "-R", to.User()}, paths...)
		to.Exec(ctx, "root", nil, nil, nil, args...)
	}
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

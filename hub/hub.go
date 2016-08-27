package hub

import (
	"fmt"
	"os"
	"path/filepath"
	"sort"
	"strings"

	"github.com/cloudway/platform/config"
	"github.com/cloudway/platform/pkg/archive"
	"github.com/cloudway/platform/pkg/files"
	"github.com/cloudway/platform/pkg/manifest"
)

type PluginHub struct {
	installDir string
}

func New() (*PluginHub, error) {
	dir := config.GetOrDefault("hub.dir", "/var/lib/cloudway/plugins")
	if err := os.MkdirAll(dir, 0755); err != nil {
		return nil, err
	}
	return &PluginHub{dir}, nil
}

func (hub *PluginHub) ListPlugins(namespace string, category manifest.Category) []*manifest.Plugin {
	dir, err := os.Open(hub.getBaseDir(namespace, ""))
	if err != nil {
		return nil
	}
	defer dir.Close()

	names, err := dir.Readdirnames(0)
	if err != nil {
		return nil
	}

	var result []*manifest.Plugin
	for _, tag := range names {
		if namespace != "" {
			tag = namespace + "/" + tag
		}
		meta, err := hub.GetPluginInfo(tag)
		if err == nil && (category == "" || category == meta.Category) {
			result = append(result, meta)
		}
	}
	sort.Sort(byDisplayName(result))
	return result
}

type byDisplayName []*manifest.Plugin

func (a byDisplayName) Len() int           { return len(a) }
func (a byDisplayName) Swap(i, j int)      { a[i], a[j] = a[j], a[i] }
func (a byDisplayName) Less(i, j int) bool { return a[i].DisplayName < a[j].DisplayName }

func (hub *PluginHub) GetPluginPath(tag string) (string, error) {
	_, namespace, name, version := parseTag(tag)
	return hub.pluginPath(namespace, name, version)
}

func (hub *PluginHub) pluginPath(namespace, name, version string) (string, error) {
	base := hub.getBaseDir(namespace, name)
	versions, err := getAllVersions(base)
	if err != nil {
		if os.IsNotExist(err) {
			err = fmt.Errorf("Plugin not found: %s", name)
		}
		return "", err
	}

	actualVersion := matchVersion(versions, version)
	if actualVersion == "" {
		return "", fmt.Errorf("Version not found: %s", version)
	}
	return filepath.Join(base, actualVersion), nil
}

func (hub *PluginHub) GetPluginInfo(tag string) (*manifest.Plugin, error) {
	_, plugin, err := hub.GetPluginInfoWithName(tag)
	return plugin, err
}

func (hub *PluginHub) GetPluginInfoWithName(tag string) (string, *manifest.Plugin, error) {
	service, namespace, name, version := parseTag(tag)
	path, err := hub.pluginPath(namespace, name, version)
	if err != nil {
		return "", nil, err
	} else {
		plugin, err := archive.ReadManifest(path)
		return service, plugin, err
	}
}

func (hub *PluginHub) InstallPlugin(namespace string, path string) (err error) {
	meta, err := archive.ReadManifest(path)
	if err != nil {
		return err
	}

	installDir := filepath.Join(hub.getBaseDir(namespace, meta.Name), meta.Version)
	if err = os.RemoveAll(installDir); err != nil {
		return err
	}
	if err = os.MkdirAll(installDir, 0755); err != nil {
		return err
	}

	if fi, _ := os.Stat(path); fi.IsDir() {
		return files.CopyFiles(path, installDir)
	} else {
		return files.ExtractFiles(path, installDir)
	}
}

func (hub *PluginHub) RemovePlugin(tag string) error {
	_, namespace, name, version := parseTag(tag)
	base := hub.getBaseDir(namespace, name)
	if version == "" {
		return os.RemoveAll(base)
	} else {
		return os.RemoveAll(filepath.Join(base, version))
	}
}

func (hub *PluginHub) getBaseDir(namespace, name string) string {
	if namespace == "" {
		namespace = "_"
	}
	dir := filepath.Join(hub.installDir, namespace)
	if name != "" {
		dir = filepath.Join(dir, name)
	}
	return dir
}

func parseTag(tag string) (service, namespace, name, version string) {
	parts := strings.SplitN(tag, "=", 2)
	if len(parts) == 2 {
		service, tag = parts[0], parts[1]
	}

	parts = strings.SplitN(tag, "/", 2)
	if len(parts) == 2 {
		namespace, tag = parts[0], parts[1]
	}

	parts = strings.SplitN(tag, ":", 2)
	if len(parts) == 2 {
		name, version = parts[0], parts[1]
	} else {
		name, version = parts[0], ""
	}

	return
}

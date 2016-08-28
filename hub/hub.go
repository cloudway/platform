package hub

import (
	"fmt"
	"net/http"
	"os"
	"path/filepath"
	"regexp"
	"sort"

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
	dir, err := os.Open(hub.getBaseDir(namespace, "", ""))
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
	_, namespace, name, version, err := ParseTag(tag)
	if err != nil {
		return "", err
	}
	return hub.pluginPath(namespace, name, version)
}

func (hub *PluginHub) pluginPath(namespace, name, version string) (string, error) {
	base := hub.getBaseDir(namespace, name, "")
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
	service, namespace, name, version, err := ParseTag(tag)
	if err != nil {
		return "", nil, err
	}

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

	if meta.Name == "" || meta.Version == "" || meta.Category == "" || meta.BaseImage == "" {
		return invalidManifestErr{}
	}

	tag := meta.Name + ":" + meta.Version
	if namespace != "" {
		tag = namespace + "/" + tag
	}
	if _, _, _, _, err = ParseTag(tag); err != nil {
		return invalidManifestErr{}
	}

	installDir := hub.getBaseDir(namespace, meta.Name, meta.Version)
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
	_, namespace, name, version, err := ParseTag(tag)
	if err != nil {
		return err
	}
	dir := hub.getBaseDir(namespace, name, version)
	if _, err := os.Stat(dir); err != nil {
		return err
	}
	return os.RemoveAll(dir)
}

func (hub *PluginHub) getBaseDir(namespace, name, version string) string {
	if namespace == "" {
		namespace = "_"
	}
	dir := filepath.Join(hub.installDir, namespace)
	if name != "" {
		dir = filepath.Join(dir, name)
	}
	if version != "" {
		dir = filepath.Join(dir, version)
	}
	return dir
}

var tagPattern = regexp.MustCompile(`^([a-zA-Z_0-9]+=)?([a-zA-Z_0-9]+/)?([a-zA-Z_0-9]+)(:[0-9][[0-9.]*)?$`)

func ParseTag(tag string) (service, namespace, name, version string, err error) {
	m := tagPattern.FindStringSubmatch(tag)
	if len(m) == 0 {
		err = malformedTagError(tag)
		return
	}

	service, namespace, name, version = m[1], m[2], m[3], m[4]
	if service != "" {
		service = service[:len(service)-1]
	}
	if namespace != "" {
		namespace = namespace[:len(namespace)-1]
	}
	if version != "" {
		version = version[1:]
	}
	return
}

// Errors

type invalidManifestErr struct{}

func (e invalidManifestErr) Error() string {
	return "invalid plugin manifest"
}

func (e invalidManifestErr) HTTPErrorStatusCode() int {
	return http.StatusBadRequest
}

type malformedTagError string

func (e malformedTagError) Error() string {
	return fmt.Sprintf("%s: malformed plugin tag", string(e))
}

func (e malformedTagError) HTTPErrorStatusCode() int {
	return http.StatusBadRequest
}

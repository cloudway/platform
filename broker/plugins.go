package broker

import (
	"io"
	"io/ioutil"
	"os"

	"github.com/cloudway/platform/pkg/manifest"
)

// GetInstalledPlugins returns all installed plugins, include user and system plugins.
func (br *UserBroker) GetInstalledPlugins(category manifest.Category) (plugins []*manifest.Plugin) {
	// get system plugins
	plugins = br.Hub.ListPlugins("", category)

	// get user defined plugins
	user := br.Hub.ListPlugins(br.Namespace(), category)
	if len(user) == 0 {
		return
	}

	// override system plugin with user defined plugin
	for i, p := range plugins {
		for j, pp := range user {
			if pp.Name == p.Name {
				plugins[i] = pp
				user = append(user[:j], user[j+1:]...)
				break
			}
		}
	}
	return append(plugins, user...)
}

// GetUserPlugins returns a list of user defined plugins.
func (br *UserBroker) GetUserPlugins(category manifest.Category) (plugins []*manifest.Plugin) {
	return br.Hub.ListPlugins(br.Namespace(), category)
}

// GetPluginInfo returns a installed plugin meta data.
func (br *UserBroker) GetPluginInfo(tag string) (plugin *manifest.Plugin, err error) {
	// get the user defined plugin, if it's not found then get system plugin
	plugin, err = br.Hub.GetPluginInfo(br.Namespace() + "/" + tag)
	if err != nil {
		plugin, err = br.Hub.GetPluginInfo("_/" + tag)
	}
	return
}

// InstallPlugin installs a user defined plugin.
func (br *UserBroker) InstallPlugin(ar io.Reader) error {
	tempfile, err := ioutil.TempFile("", "plugin")
	if err != nil {
		return err
	}
	defer os.Remove(tempfile.Name())

	_, err = io.Copy(tempfile, ar)
	tempfile.Close()
	if err != nil {
		return err
	}
	return br.Hub.InstallPlugin(br.Namespace(), tempfile.Name())
}

// RemovePlugin removes a user defined plugin.
func (br *UserBroker) RemovePlugin(tag string) error {
	return br.Hub.RemovePlugin(br.Namespace() + "/" + tag)
}

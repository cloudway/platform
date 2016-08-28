package broker

import (
	"fmt"
	"io"
	"io/ioutil"
	"os"
	"sort"

	"github.com/cloudway/platform/hub"
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

	// sort plugins by display name
	plugins = append(plugins, user...)
	sort.Sort(byDisplayName(plugins))
	return plugins
}

type byDisplayName []*manifest.Plugin

func (a byDisplayName) Len() int           { return len(a) }
func (a byDisplayName) Swap(i, j int)      { a[i], a[j] = a[j], a[i] }
func (a byDisplayName) Less(i, j int) bool { return a[i].DisplayName < a[j].DisplayName }

// GetUserPlugins returns a list of user defined plugins.
func (br *UserBroker) GetUserPlugins(category manifest.Category) (plugins []*manifest.Plugin) {
	return br.Hub.ListPlugins(br.Namespace(), category)
}

// GetPluginInfo returns a installed plugin meta data.
func (br *UserBroker) GetPluginInfo(tag string) (plugin *manifest.Plugin, err error) {
	_, plugin, err = br.getPluginInfoWithNames(&tag)
	return
}

// GetPluginInfoWithName is a helper method to get plugin info with service name.
func (br *UserBroker) getPluginInfoWithNames(tag *string) (service string, plugin *manifest.Plugin, err error) {
	service, namespace, name, version, err := hub.ParseTag(*tag)
	if err != nil {
		return
	}

	cleanTag := name
	if namespace != "" {
		cleanTag = namespace + "/" + cleanTag
	}
	if version != "" {
		cleanTag += ":" + version
	}

	if namespace != "" {
		// get the shared user defined plugin
		plugin, err = br.Hub.GetPluginInfo(cleanTag)
		if err == nil && !plugin.Shared && namespace != br.Namespace() {
			err = fmt.Errorf("%s: plugin not found", cleanTag)
		}
	} else {
		// get the user defined plugin, if it's not found then get system plugin
		namespace = br.Namespace()
		plugin, err = br.Hub.GetPluginInfo(namespace + "/" + cleanTag)
		if err != nil {
			namespace = ""
			plugin, err = br.Hub.GetPluginInfo("_/" + cleanTag)
		}
	}

	if err == nil {
		*tag = plugin.Name + ":" + plugin.Version
		if namespace != "" {
			*tag = namespace + "/" + *tag
		}
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

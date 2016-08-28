package api_test

import (
	"io/ioutil"
	"os"
	"path/filepath"

	. "github.com/onsi/ginkgo"
	. "github.com/onsi/gomega"

	"golang.org/x/net/context"
	"gopkg.in/yaml.v2"

	"github.com/cloudway/platform/pkg/manifest"
)

const (
	PUBLIC_PLUGIN  = "api_test_public"
	PRIVATE_PLUGIN = "api_test_private"
)

func installMockPlugins() {
	meta := &manifest.Plugin{
		Name:        "api_test",
		DisplayName: "Mock plugin",
		Description: "A mock plugin for testing purposes",
		Version:     "1.0",
		Vendor:      "cloudway",
		Category:    "mock",
		BaseImage:   "busybox",
		Endpoints: []*manifest.Endpoint{{
			PrivateHostName: "HOST",
			PrivatePortName: "PORT",
			PrivatePort:     8080,
			ProxyMappings: []*manifest.ProxyMapping{{
				Frontend:  "",
				Backend:   "",
				Protocols: []string{"http"},
			}},
		}},
	}

	// Create public and private plugins
	versions := []string{"1.0", "1.0.1", "1.0.3", "1.2.5", "1.10.1", "2.3", "2.5"}
	for _, v := range versions {
		meta.Name = PUBLIC_PLUGIN
		meta.Version = v
		meta.Vendor = "public"
		Ω(installMockPlugin("", meta)).Should(Succeed())

		meta.Name = PRIVATE_PLUGIN
		meta.Version = v
		meta.Vendor = "private"
		Ω(installMockPlugin(TEST_NAMESPACE, meta)).Should(Succeed())
	}
}

func installMockPlugin(namespace string, meta *manifest.Plugin) (err error) {
	path, err := ioutil.TempDir("", "plugin")
	if err != nil {
		return
	}
	defer os.RemoveAll(path)

	if err = os.Mkdir(filepath.Join(path, "manifest"), 0777); err != nil {
		return
	}

	data, err := yaml.Marshal(meta)
	if err != nil {
		return
	}

	filename := filepath.Join(path, "manifest", "plugin.yml")
	if err = ioutil.WriteFile(filename, data, 0666); err != nil {
		return err
	}

	return broker.Hub.InstallPlugin(namespace, path)
}

func removeMockPlugins() {
	broker.Hub.RemovePlugin(PUBLIC_PLUGIN)
	broker.Hub.RemovePlugin(TEST_NAMESPACE + "/" + PRIVATE_PLUGIN)
}

func getTags(plugins []*manifest.Plugin) (tags []string) {
	for _, p := range plugins {
		if p.Category == "mock" {
			tags = append(tags, p.Name)
		}
	}
	return
}

var _ = Describe("Plugins", func() {
	var cli *TestClient
	var ctx = context.Background()

	BeforeEach(func() {
		cli = NewTestClient()
	})

	AfterEach(func() {
		cli.Close()
	})

	It("should success to list all public plugins", func() {
		plugins, err := cli.ListPlugins(ctx, "", "")
		Ω(err).ShouldNot(HaveOccurred())
		Ω(getTags(plugins)).Should(ConsistOf(PUBLIC_PLUGIN))
	})

	It("should success to list all private plugins", func() {
		plugins, err := cli.ListPlugins(ctx, TEST_NAMESPACE, "")
		Ω(err).ShouldNot(HaveOccurred())
		Ω(getTags(plugins)).Should(ConsistOf(PRIVATE_PLUGIN))
	})

	It("should success to get public plugin info", func() {
		plugin, err := cli.GetPluginInfo(ctx, PUBLIC_PLUGIN)
		Ω(err).ShouldNot(HaveOccurred())
		Ω(plugin.Name).Should(Equal(PUBLIC_PLUGIN))
		Ω(plugin.Vendor).Should(Equal("public"))
	})

	It("should success to get private plugin info", func() {
		plugin, err := cli.GetPluginInfo(ctx, TEST_NAMESPACE+"/"+PRIVATE_PLUGIN)
		Ω(err).ShouldNot(HaveOccurred())
		Ω(plugin.Name).Should(Equal(PRIVATE_PLUGIN))
		Ω(plugin.Vendor).Should(Equal("private"))
	})

	It("should success to get highest version of plugin", func() {
		plugin, err := cli.GetPluginInfo(ctx, PUBLIC_PLUGIN+":1.0")
		Ω(err).ShouldNot(HaveOccurred())
		Ω(plugin.Version).Should(Equal("1.0.3"))

		plugin, err = cli.GetPluginInfo(ctx, PUBLIC_PLUGIN+":1")
		Ω(err).ShouldNot(HaveOccurred())
		Ω(plugin.Version).Should(Equal("1.10.1"))
	})

	It("should fail if plugin not found", func() {
		_, err := cli.GetPluginInfo(ctx, "nonexist")
		Ω(err).Should(HaveOccurred())
	})

	It("should fail if the given version of plugin not found", func() {
		_, err := cli.GetPluginInfo(ctx, PUBLIC_PLUGIN+":10")
		Ω(err).Should(HaveOccurred())
	})
})

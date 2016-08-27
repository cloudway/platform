package hub

import (
	"io/ioutil"
	"os"
	"path/filepath"
	"testing"

	. "github.com/onsi/ginkgo"
	. "github.com/onsi/ginkgo/extensions/table"
	. "github.com/onsi/gomega"

	"gopkg.in/yaml.v2"

	"github.com/cloudway/platform/pkg/manifest"
)

func TestHub(t *testing.T) {
	RegisterFailHandler(Fail)
	RunSpecs(t, "Hub Suite")
}

var pluginHub *PluginHub

var _ = BeforeSuite(func() {
	testdir, err := ioutil.TempDir("", "hub")
	Ω(err).ShouldNot(HaveOccurred())
	pluginHub = &PluginHub{testdir}
})

var _ = AfterSuite(func() {
	os.RemoveAll(pluginHub.installDir)
})

func emptyTestDir() error {
	testdir := pluginHub.installDir

	dir, err := os.Open(testdir)
	if err != nil {
		return nil
	}
	defer dir.Close()

	subs, err := dir.Readdirnames(0)
	if err != nil {
		return nil
	}

	for _, sub := range subs {
		if err = os.RemoveAll(filepath.Join(testdir, sub)); err != nil {
			return err
		}
	}

	return nil
}

func makeMockPlugin(meta *manifest.Plugin) (path string, err error) {
	path, err = ioutil.TempDir("", "plugin")
	if err != nil {
		return
	}

	if err = os.Mkdir(filepath.Join(path, "manifest"), 0777); err != nil {
		return
	}

	data, err := yaml.Marshal(meta)
	if err != nil {
		return
	}

	filename := filepath.Join(path, "manifest", "plugin.yml")
	err = ioutil.WriteFile(filename, data, 0666)
	return
}

func getTags(plugins []*manifest.Plugin) (tags []string) {
	for _, p := range plugins {
		tags = append(tags, p.Name)
	}
	return
}

func getTagsWithVersion(plugins []*manifest.Plugin) (tags []string) {
	for _, p := range plugins {
		tags = append(tags, p.Name+":"+p.Version)
	}
	return
}

var _ = Describe("Hub", func() {
	var meta *manifest.Plugin

	BeforeEach(func() {
		meta = &manifest.Plugin{
			Name:        "mock",
			DisplayName: "Mock plugin",
			Description: "A mock plugin for testing purposes",
			Version:     "1.0",
			Vendor:      "cloudway",
			Category:    manifest.Framework,
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
	})

	AfterEach(func() {
		emptyTestDir()
	})

	var install = func(namespace string, meta *manifest.Plugin) {
		path, err := makeMockPlugin(meta)
		ExpectWithOffset(1, err).NotTo(HaveOccurred())
		defer os.RemoveAll(path)
		ExpectWithOffset(1, pluginHub.InstallPlugin(namespace, path)).To(Succeed())
	}

	Describe("List plugins", func() {
		It("should list all installed plugins", func() {
			meta.Name = "mockA"
			install("", meta)
			meta.Name = "mockB"
			install("", meta)
			meta.Name = "mockC"
			install("", meta)

			plugins := pluginHub.ListPlugins("", "")
			Ω(getTags(plugins)).Should(ConsistOf("mockA", "mockB", "mockC"))
		})

		It("should list all installed plugins with highest version", func() {
			meta.Name = "mockA"
			meta.Version = "1.0"
			install("", meta)

			meta.Name = "mockA"
			meta.Version = "1.0.2"
			install("", meta)

			meta.Name = "mockB"
			meta.Version = "2.3"
			install("", meta)

			meta.Name = "mockB"
			meta.Version = "3.4"
			install("", meta)

			plugins := pluginHub.ListPlugins("", "")
			Ω(getTagsWithVersion(plugins)).Should(ConsistOf("mockA:1.0.2", "mockB:3.4"))
		})

		It("should list all installed plugins in the given namespace", func() {
			meta.Name = "public"
			install("", meta)

			meta.Name = "private"
			install("private", meta)

			plugins := pluginHub.ListPlugins("", "")
			Ω(getTags(plugins)).Should(ConsistOf("public"))

			plugins = pluginHub.ListPlugins("private", "")
			Ω(getTags(plugins)).Should(ConsistOf("private"))

			plugins = pluginHub.ListPlugins("nonexist", "")
			Ω(plugins).Should(BeEmpty())
		})

		It("should list all installed plugins for the give category", func() {
			meta.Name = "framework"
			meta.Category = manifest.Framework
			install("", meta)

			meta.Name = "service"
			meta.Category = manifest.Service
			install("", meta)

			plugins := pluginHub.ListPlugins("", manifest.Framework)
			Ω(getTags(plugins)).Should(ConsistOf("framework"))

			plugins = pluginHub.ListPlugins("", manifest.Service)
			Ω(getTags(plugins)).Should(ConsistOf("service"))

			plugins = pluginHub.ListPlugins("", "")
			Ω(getTags(plugins)).Should(ConsistOf("framework", "service"))
		})
	})

	Describe("Get plugin info", func() {
		It("should return the plugin information", func() {
			install("", meta)

			plugin, err := pluginHub.GetPluginInfo("mock")
			Ω(err).ShouldNot(HaveOccurred())
			plugin.Path = ""
			Ω(plugin).Should(Equal(meta))
		})

		It("should fail if the plugin not found", func() {
			install("", meta)

			_, err := pluginHub.GetPluginInfo("nonexist")
			Ω(err).Should(HaveOccurred())
		})

		It("should return plugin in the given namespace", func() {
			meta.Name = "public"
			install("", meta)

			meta.Name = "private"
			install("private", meta)

			plugin, err := pluginHub.GetPluginInfo("public")
			Ω(err).ShouldNot(HaveOccurred())
			Ω(plugin.Name).Should(Equal("public"))

			plugin, err = pluginHub.GetPluginInfo("private/private")
			Ω(err).ShouldNot(HaveOccurred())
			Ω(plugin.Name).Should(Equal("private"))

			_, err = pluginHub.GetPluginInfo("nonexist/private")
			Ω(err).Should(HaveOccurred())
		})

		It("should return the plugin with highest version", func() {
			var assertVersion = func(newVersion, highestVersion string) {
				meta.Version = newVersion
				install("", meta)

				plugin, err := pluginHub.GetPluginInfo("mock")
				ExpectWithOffset(1, err).NotTo(HaveOccurred())
				ExpectWithOffset(1, plugin.Version).To(Equal(highestVersion))
			}

			assertVersion("1.0", "1.0")
			assertVersion("1.0.1", "1.0.1")
			assertVersion("1.1.5", "1.1.5")
			assertVersion("1.1.2", "1.1.5")
			assertVersion("1.10.2", "1.10.2")
			assertVersion("1.2.3", "1.10.2")
		})

		Context("version matching", func() {
			BeforeEach(func() {
				versions := []string{
					"1.0", "1.0.1", "1.1.5", "1.10.2", "2.0.2", "2.0.3", "2.4",
				}
				for _, ver := range versions {
					meta.Version = ver
					install("", meta)
				}
			})

			DescribeTable("should return the plugin matching the given version",
				func(given, expected string) {
					plugin, err := pluginHub.GetPluginInfo("mock:" + given)
					Ω(err).ShouldNot(HaveOccurred())
					Ω(plugin.Version).Should(Equal(expected))
				},

				Entry("1.0 -> 1.0.1", "1.0", "1.0.1"),
				Entry("1.0.1 -> 1.0.1", "1.0.1", "1.0.1"),
				Entry("1.1 -> 1.1.5", "1.1", "1.1.5"),
				Entry("2.0 -> 2.0.3", "2.0", "2.0.3"),
				Entry("1 -> 1.10.2", "1", "1.10.2"),
				Entry("2 -> 2.4", "2", "2.4"),
			)
		})

		It("should fail if the given version does not match any installed version", func() {
			install("", meta)

			_, err := pluginHub.GetPluginInfo("mock:2.0")
			Ω(err).Should(HaveOccurred())
		})
	})

	Describe("Remove plugin", func() {
		It("should remove installed plugin", func() {
			install("", meta)

			_, err := pluginHub.GetPluginInfo("mock")
			Ω(err).ShouldNot(HaveOccurred())
			_, err = pluginHub.GetPluginInfo("mock:1.0")
			Ω(err).ShouldNot(HaveOccurred())

			Ω(pluginHub.RemovePlugin("mock")).Should(Succeed())

			_, err = pluginHub.GetPluginInfo("mock")
			Ω(err).Should(HaveOccurred())
			_, err = pluginHub.GetPluginInfo("mock:1.0")
			Ω(err).Should(HaveOccurred())
		})

		It("should not fail if plugin not found", func() {
			Ω(pluginHub.RemovePlugin("mock")).Should(Succeed())
		})

		It("should remove given version of a plugin", func() {
			versions := []string{"1.0", "1.0.5", "1.0.2", "1.10.1"}
			for _, v := range versions {
				meta.Version = v
				install("", meta)
			}

			plugin, err := pluginHub.GetPluginInfo("mock:1.0")
			Ω(err).ShouldNot(HaveOccurred())
			Ω(plugin.Version).Should(Equal("1.0.5"))

			Ω(pluginHub.RemovePlugin("mock:1.0.5")).Should(Succeed())

			_, err = pluginHub.GetPluginInfo("mock:1.0.5")
			Ω(err).Should(HaveOccurred())

			plugin, err = pluginHub.GetPluginInfo("mock:1.0")
			Ω(err).ShouldNot(HaveOccurred())
			Ω(plugin.Version).Should(Equal("1.0.2"))
		})

		It("should not fail if the given version of a plugin not found", func() {
			install("", meta)
			Ω(pluginHub.RemovePlugin("mock:2.0")).Should(Succeed())
		})

		It("should remove all versions of a plugin", func() {
			versions := []string{"1.0", "1.0.5", "1.0.2", "1.10.1"}
			for _, v := range versions {
				meta.Version = v
				install("", meta)
			}

			Ω(pluginHub.RemovePlugin("mock")).Should(Succeed())
			_, err := pluginHub.GetPluginInfo("mock")
			Ω(err).Should(HaveOccurred())
		})
	})
})

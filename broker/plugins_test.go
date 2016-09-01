package broker_test

import (
	"archive/tar"
	"bytes"
	"io/ioutil"
	"os"
	"path/filepath"

	. "github.com/onsi/ginkgo"
	. "github.com/onsi/gomega"

	"golang.org/x/net/context"
	"gopkg.in/yaml.v2"

	"github.com/cloudway/platform/auth/userdb"
	br "github.com/cloudway/platform/broker"
	"github.com/cloudway/platform/config"
	"github.com/cloudway/platform/hub"
	"github.com/cloudway/platform/pkg/archive"
	"github.com/cloudway/platform/pkg/manifest"
)

var _ = Describe("Plugins", func() {
	var user = userdb.BasicUser{
		Name:      TESTUSER,
		Namespace: NAMESPACE,
	}

	var (
		br         *br.UserBroker
		originHub  *hub.PluginHub
		hubdir     string
		testhubdir string
	)

	BeforeEach(func() {
		var err error

		Ω(broker.CreateUser(&user, "test")).Should(Succeed())
		br = broker.NewUserBroker(&user, context.Background())

		// retrieve original mock plugins
		mockPath, err := broker.Hub.GetPluginPath("mock")
		Ω(err).ShouldNot(HaveOccurred())
		mockdbPath, err := broker.Hub.GetPluginPath("mockdb")
		Ω(err).ShouldNot(HaveOccurred())

		// use a temporary directory as the test hub directory
		hubdir = config.Get("hub.dir")
		testhubdir, err = ioutil.TempDir("", "hub")
		Ω(err).ShouldNot(HaveOccurred())
		config.Set("hub.dir", testhubdir)

		// mock the plugin hub
		testhub, err := hub.New()
		Ω(err).ShouldNot(HaveOccurred())
		originHub = broker.Hub
		broker.Hub = testhub

		// install mock plugins in test hub
		Ω(broker.Hub.InstallPlugin("", mockPath)).Should(Succeed())
		Ω(broker.Hub.InstallPlugin("", mockdbPath)).Should(Succeed())
	})

	AfterEach(func() {
		broker.Hub = originHub
		config.Set("hub.dir", hubdir)
		os.RemoveAll(testhubdir)
		Ω(broker.RemoveUser(TESTUSER)).Should(Succeed())
	})

	var preparePlugin = func(meta *manifest.Plugin) (path string, err error) {
		path, err = ioutil.TempDir("", "plugin")
		if err != nil {
			return
		}

		if err = os.Mkdir(filepath.Join(path, "manifest"), 0777); err != nil {
			return
		}

		if meta != nil {
			data, err := yaml.Marshal(meta)
			if err != nil {
				return "", err
			}

			filename := filepath.Join(path, "manifest", "plugin.yml")
			if err = ioutil.WriteFile(filename, data, 0666); err != nil {
				return "", err
			}
		}

		return
	}

	var installPlugin = func(namespace string, meta *manifest.Plugin) error {
		path, err := preparePlugin(meta)
		if err == nil {
			err = broker.Hub.InstallPlugin(namespace, path)
			os.RemoveAll(path)
		}
		return err
	}

	var installTestPlugins = func() {
		var meta = manifest.Plugin{
			Name:        "test",
			DisplayName: "Test Plugin",
			Version:     "1.0",
			Vendor:      "test",
			Category:    manifest.Framework,
			BaseImage:   "busybox",
		}
		Ω(installPlugin(NAMESPACE, &meta)).Should(Succeed())

		meta = manifest.Plugin{
			Name:        "mock",
			DisplayName: "Mock Plugin",
			Version:     "1.0",
			Vendor:      "test",
			Category:    manifest.Framework,
			BaseImage:   "busybox",
		}
		Ω(installPlugin(NAMESPACE, &meta)).Should(Succeed())

		meta = manifest.Plugin{
			Name:        "mock",
			DisplayName: "Mock Plugin",
			Version:     "1.0",
			Vendor:      "other",
			Category:    manifest.Framework,
			BaseImage:   "busybox",
		}
		Ω(installPlugin("other", &meta)).Should(Succeed())

		meta = manifest.Plugin{
			Name:        "shared",
			DisplayName: "Shared Plugin",
			Version:     "1.0",
			Vendor:      "other",
			Shared:      true,
			Category:    manifest.Framework,
			BaseImage:   "busybox",
		}
		Ω(installPlugin("other", &meta)).Should(Succeed())
	}

	var install = func(meta *manifest.Plugin) error {
		path, err := preparePlugin(meta)
		if err != nil {
			return err
		}
		defer os.RemoveAll(path)

		buf := &bytes.Buffer{}
		tw := tar.NewWriter(buf)
		if err = archive.CopyFileTree(tw, "", path, nil, false); err != nil {
			return err
		}
		tw.Close()

		return br.InstallPlugin(buf)
	}

	var getTags = func(plugins []*manifest.Plugin) []string {
		var tags []string
		for _, p := range plugins {
			tags = append(tags, p.Name)
		}
		return tags
	}

	Describe("Get Installed Plugins", func() {
		Context("without user defined plugins", func() {
			It("should return all system plugins", func() {
				plugins := br.GetInstalledPlugins("")
				Ω(getTags(plugins)).Should(ConsistOf("mock", "mockdb"))
			})

			It("should return system plugin meta data", func() {
				plugin, err := br.GetPluginInfo("mock")
				Ω(err).ShouldNot(HaveOccurred())
				Ω(plugin.Vendor).ShouldNot(Equal("test"))
			})

			It("should no user defined plugins", func() {
				Ω(br.GetUserPlugins("")).Should(BeEmpty())
			})

			It("should fail if plugin not found", func() {
				_, err := br.GetPluginInfo("mysql")
				Ω(err).Should(HaveOccurred())
			})
		})

		Context("with user defined plugins", func() {
			BeforeEach(installTestPlugins)

			It("should return all system and user plugins", func() {
				plugins := br.GetInstalledPlugins("")
				Ω(getTags(plugins)).Should(ConsistOf("mock", "mockdb", "test"))
			})

			It("should return all user defined plugins", func() {
				plugins := br.GetUserPlugins("")
				Ω(getTags(plugins)).Should(ConsistOf("mock", "test"))
			})

			It("should return system plugin meta data", func() {
				plugin, err := br.GetPluginInfo("mockdb")
				Ω(err).ShouldNot(HaveOccurred())
				Ω(plugin.Vendor).ShouldNot(Equal("test"))
			})

			It("should return user defined plugin meta data", func() {
				plugin, err := br.GetPluginInfo("test")
				Ω(err).ShouldNot(HaveOccurred())
				Ω(plugin.Vendor).Should(Equal("test"))
			})

			It("should override system plugin with user defined plugin", func() {
				plugin, err := br.GetPluginInfo("mock")
				Ω(err).ShouldNot(HaveOccurred())
				Ω(plugin.Vendor).Should(Equal("test"))
			})

			It("should fail if plugin not found", func() {
				_, err := br.GetPluginInfo("mysql")
				Ω(err).Should(HaveOccurred())
			})

			It("should not see other user's plugin", func() {
				_, err := br.GetPluginInfo("other/mock")
				Ω(err).Should(HaveOccurred())
			})

			It("should see other user's shared plugin", func() {
				plugin, err := br.GetPluginInfo("other/shared")
				Ω(err).ShouldNot(HaveOccurred())
				Ω(plugin.Vendor).Should(Equal("other"))
			})
		})
	})

	Describe("Install Plugin", func() {
		var meta *manifest.Plugin

		BeforeEach(func() {
			meta = &manifest.Plugin{
				Name:        "test",
				DisplayName: "Test Plugin",
				Version:     "1.0",
				Vendor:      "test",
				Category:    manifest.Framework,
				BaseImage:   "busybox",
			}
		})

		It("should success to install user defined plugin", func() {
			Ω(getTags(br.GetInstalledPlugins(""))).Should(ConsistOf("mock", "mockdb"))
			Ω(install(meta)).Should(Succeed())
			Ω(getTags(br.GetInstalledPlugins(""))).Should(ConsistOf("mock", "mockdb", "test"))
		})

		It("should fail to install invalid plugin", func() {
			Ω(install(nil)).ShouldNot(Succeed())
		})

		It("should fail to install with invalid plugin name", func() {
			meta.Name = ""
			Ω(install(meta)).ShouldNot(Succeed())

			meta.Name = ".."
			Ω(install(meta)).ShouldNot(Succeed())

			meta.Name = "foo/bar"
			Ω(install(meta)).ShouldNot(Succeed())
		})

		It("should fail to install with invalid plugin version", func() {
			meta.Version = ""
			Ω(install(meta)).ShouldNot(Succeed())

			meta.Version = "v1.0"
			Ω(install(meta)).ShouldNot(Succeed())
		})

		It("should fail to install with invalid plugin category", func() {
			meta.Category = ""
			Ω(install(meta)).ShouldNot(Succeed())
		})

		It("should fail to install with invalid base image", func() {
			meta.BaseImage = ""
			Ω(install(meta)).ShouldNot(Succeed())
		})
	})

	Describe("Remove Plugin", func() {
		BeforeEach(installTestPlugins)

		It("should success to remove user defined plugin", func() {
			Ω(br.RemovePlugin("test")).Should(Succeed())

			Ω(getTags(br.GetInstalledPlugins(""))).Should(ConsistOf("mock", "mockdb"))
			Ω(getTags(br.GetUserPlugins(""))).Should(ConsistOf("mock"))
			_, err := br.GetPluginInfo("test")
			Ω(err).Should(HaveOccurred())
		})

		It("should fail to remove system plugin", func() {
			Ω(br.RemovePlugin("mockdb")).ShouldNot(Succeed())
		})

		It("should fail to remove other user's plugin", func() {
			Ω(br.RemovePlugin("other/mock")).ShouldNot(Succeed())
			Ω(br.RemovePlugin("other/shared")).ShouldNot(Succeed())
		})

		It("should global plugin no longer override by user defined plugin", func() {
			Ω(br.RemovePlugin("mock")).Should(Succeed())

			plugin, err := br.GetPluginInfo("mock")
			Ω(err).ShouldNot(HaveOccurred())
			Ω(plugin.Vendor).ShouldNot(Equal("test"))
		})
	})

	Context("when namespace was not set", func() {
		BeforeEach(func() {
			br.User.Basic().Namespace = ""
			installTestPlugins()
		})

		It("should success to get system plugins", func() {
			plugins := br.GetInstalledPlugins("")
			Ω(getTags(plugins)).Should(ConsistOf("mock", "mockdb"))
		})

		It("should fail to get user defined plugins", func() {
			plugins := br.GetUserPlugins("")
			Ω(plugins).Should(BeEmpty())
		})

		It("should success to get system plugin info", func() {
			_, err := br.GetPluginInfo("mock")
			Ω(err).ShouldNot(HaveOccurred())
		})

		It("should not see other user's plugin", func() {
			_, err := br.GetPluginInfo("other/mock")
			Ω(err).Should(HaveOccurred())
		})

		It("should see other user's shared plugin", func() {
			plugin, err := br.GetPluginInfo("other/shared")
			Ω(err).ShouldNot(HaveOccurred())
			Ω(plugin.Vendor).Should(Equal("other"))
		})

		It("should fail to install plugin", func() {
			meta := &manifest.Plugin{
				Name:        "test",
				DisplayName: "Test Plugin",
				Version:     "1.0",
				Vendor:      "test",
				Category:    manifest.Framework,
				BaseImage:   "busybox",
			}

			Ω(install(meta)).ShouldNot(Succeed())
		})

		It("should fail to remove plugin", func() {
			Ω(br.RemovePlugin("mock")).ShouldNot(Succeed())
		})
	})
})

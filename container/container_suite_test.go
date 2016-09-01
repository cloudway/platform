package container_test

import (
	"testing"

	. "github.com/onsi/ginkgo"
	. "github.com/onsi/gomega"

	"github.com/cloudway/platform/config"
	"github.com/cloudway/platform/container"
	"github.com/cloudway/platform/hub"
	_ "github.com/cloudway/platform/scm/mock"
	"golang.org/x/net/context"
)

var (
	dockerCli container.DockerClient
	pluginHub *hub.PluginHub
)

func TestContainer(t *testing.T) {
	RegisterFailHandler(Fail)
	RunSpecs(t, "Container Suite")
}

var _ = BeforeSuite(func() {
	var err error

	Expect(config.Initialize()).To(Succeed())

	dockerCli, err = container.NewEnvClient()
	Expect(err).NotTo(HaveOccurred())

	_, err = dockerCli.ServerVersion(context.Background())
	Expect(err).NotTo(HaveOccurred())

	pluginHub, err = hub.New()
	Expect(err).NotTo(HaveOccurred())
})

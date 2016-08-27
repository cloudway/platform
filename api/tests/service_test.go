package api_test

import (
	. "github.com/onsi/ginkgo"
	. "github.com/onsi/gomega"

	"github.com/cloudway/platform/api/types"
	"golang.org/x/net/context"
)

var _ = Describe("Service", func() {
	var cli *TestClient
	var ctx = context.Background()

	BeforeEach(func() {
		cli = NewTestClientWithNamespace(true)
		opts := types.CreateApplication{
			Name:      "test",
			Framework: "php",
		}
		_, err := cli.CreateApplication(ctx, opts, nil)
		Ω(err).ShouldNot(HaveOccurred())
	})

	AfterEach(func() {
		cli.Close()
	})

	Describe("Create", func() {
		It("should success when creating new service", func() {
			Ω(cli.CreateService(ctx, nil, "test", "mysql")).Should(Succeed())
		})

		It("should success to create multiple service", func() {
			Ω(cli.CreateService(ctx, nil, "test", "mysql", "redis")).Should(Succeed())
		})

		It("should fail to create service twice", func() {
			Ω(cli.CreateService(ctx, nil, "test", "mysql")).Should(Succeed())
			Ω(cli.CreateService(ctx, nil, "test", "mysql")).ShouldNot(Succeed())
		})

		It("should success to create two service with different name", func() {
			Ω(cli.CreateService(ctx, nil, "test", "db1=mysql")).Should(Succeed())
			Ω(cli.CreateService(ctx, nil, "test", "db2=mysql")).Should(Succeed())
		})

		It("should fail if application not found", func() {
			Ω(cli.CreateService(ctx, nil, "nonexist", "mysql")).ShouldNot(Succeed())
		})
	})

	Describe("Remove", func() {
		It("should success if service exist", func() {
			Ω(cli.CreateService(ctx, nil, "test", "mysql")).Should(Succeed())
			Ω(cli.RemoveService(ctx, "test", "mysql")).Should(Succeed())
		})

		It("should fail if service does not exist", func() {
			Ω(cli.RemoveService(ctx, "test", "mysql")).ShouldNot(Succeed())
		})

		It("should fail if application does not exist", func() {
			Ω(cli.RemoveService(ctx, "nonexist", "mysql")).ShouldNot(Succeed())
		})
	})
})

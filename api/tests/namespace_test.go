package api_test

import (
	. "github.com/onsi/ginkgo"
	. "github.com/onsi/gomega"

	"github.com/cloudway/platform/api/types"
	"golang.org/x/net/context"
)

var _ = Describe("Namespace", func() {
	var cli *TestClient
	var ctx = context.Background()

	AfterEach(func() {
		cli.Close()
	})

	Describe("Get namespace", func() {
		BeforeEach(func() {
			cli = NewTestClientWithNamespace(true)
		})

		It("should returns the user namespace", func() {
			namespace, err := cli.GetNamespace(ctx)
			Expect(err).NotTo(HaveOccurred())
			Expect(namespace).To(Equal(TEST_NAMESPACE))
		})
	})

	Describe("Set namespace", func() {
		Context("with no application exists", func() {
			BeforeEach(func() {
				cli = NewTestClientWithUser(true)
			})

			It("should success to create namespace", func() {
				br := cli.NewUserBroker()

				Expect(br.Refresh()).To(Succeed())
				Expect(br.User.Basic().Namespace).To(BeEmpty())

				Expect(cli.SetNamespace(ctx, TEST_NAMESPACE)).To(Succeed())

				Expect(br.Refresh()).To(Succeed())
				Expect(br.User.Basic().Namespace).To(Equal(TEST_NAMESPACE))
			})
		})

		Context("with applications exists", func() {
			BeforeEach(func() {
				cli = NewTestClientWithNamespace(true)
				opts := types.CreateApplication{
					Name:      "test",
					Framework: "mock",
				}
				_, err := cli.CreateApplication(ctx, opts, nil)
				Expect(err).NotTo(HaveOccurred())
			})

			It("should fail to change namespace", func() {
				Expect(cli.SetNamespace(ctx, "other")).NotTo(Succeed())
			})
		})
	})

	Describe("Remove namespace", func() {
		Context("with no application exists", func() {
			BeforeEach(func() {
				cli = NewTestClientWithNamespace(true)
				br := cli.NewUserBroker()
				Expect(br.Refresh()).To(Succeed())
				Expect(br.User.Basic().Namespace).To(Equal(TEST_NAMESPACE))
			})

			AfterEach(func() {
				br := cli.NewUserBroker()
				Expect(br.Refresh()).To(Succeed())
				Expect(br.User.Basic().Namespace).To(BeEmpty())
			})

			It("should success without forced remove", func() {
				Expect(cli.RemoveNamespace(ctx, false)).To(Succeed())
			})

			It("should success with forced remove", func() {
				Expect(cli.RemoveNamespace(ctx, true)).To(Succeed())
			})
		})

		Context("with applications exists", func() {
			BeforeEach(func() {
				cli = NewTestClientWithNamespace(true)
				opts := types.CreateApplication{
					Name:      "test",
					Framework: "mock",
				}
				_, err := cli.CreateApplication(ctx, opts, nil)
				Expect(err).NotTo(HaveOccurred())
			})

			It("should fail without forced remove", func() {
				Expect(cli.RemoveNamespace(ctx, false)).NotTo(Succeed())
			})

			It("should success with forced remove", func() {
				Expect(cli.RemoveNamespace(ctx, true)).To(Succeed())
			})
		})
	})
})

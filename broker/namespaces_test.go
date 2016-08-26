package broker_test

import (
	. "github.com/onsi/ginkgo"
	. "github.com/onsi/gomega"

	"github.com/cloudway/platform/auth/userdb"
	"github.com/cloudway/platform/container"
	"golang.org/x/net/context"
)

var _ = Describe("Namespaces", func() {
	var user *userdb.BasicUser

	BeforeEach(func() {
		user = &userdb.BasicUser{Name: TESTUSER}
		Expect(broker.CreateUser(user, "test")).To(Succeed())
	})

	AfterEach(func() {
		Expect(broker.RemoveUser(TESTUSER)).To(Succeed())
	})

	var createTestApp = func() {
		br := broker.NewUserBroker(user, context.Background())
		ExpectWithOffset(1, br.CreateNamespace(NAMESPACE)).To(Succeed())

		opts := container.CreateOptions{Name: "test"}
		_, err := br.CreateApplication(opts, []string{"php"})
		ExpectWithOffset(1, err).NotTo(HaveOccurred())
	}

	Describe("Create", func() {
		Context("with fresh new user", func() {
			It("should success", func() {
				br := broker.NewUserBroker(user, context.Background())
				Expect(br.CreateNamespace(NAMESPACE)).To(Succeed())
			})

			It("should fail with empty namespace", func() {
				br := broker.NewUserBroker(user, context.Background())
				Expect(br.CreateNamespace("")).NotTo(Succeed())
			})

			It("should fail with invalid namespace", func() {
				br := broker.NewUserBroker(user, context.Background())
				Expect(br.CreateNamespace("invalid-namespace")).NotTo(Succeed())
			})
		})

		Context("when changing namespace", func() {
			BeforeEach(func() {
				br := broker.NewUserBroker(user, context.Background())
				Expect(br.CreateNamespace(NAMESPACE)).To(Succeed())
			})

			It("should success if namespace not actually changed", func() {
				br := broker.NewUserBroker(user, context.Background())
				Expect(br.CreateNamespace(NAMESPACE)).To(Succeed())
			})

			It("should success to change to another namespace", func() {
				br := broker.NewUserBroker(user, context.Background())
				Expect(br.CreateNamespace("other")).To(Succeed())
			})

			It("should fail to change to empty namespace", func() {
				br := broker.NewUserBroker(user, context.Background())
				Expect(br.CreateNamespace("")).NotTo(Succeed())
			})

			It("should fail with invalid namespace", func() {
				br := broker.NewUserBroker(user, context.Background())
				Expect(br.CreateNamespace("invalid-namespace")).NotTo(Succeed())
			})
		})

		Context("when applications exists in the namespace", func() {
			BeforeEach(createTestApp)

			It("should success if namespace is not actually changed", func() {
				br := broker.NewUserBroker(user, context.Background())
				Expect(br.CreateNamespace(NAMESPACE)).To(Succeed())
			})

			It("should fail to change to another namespace", func() {
				br := broker.NewUserBroker(user, context.Background())
				Expect(br.CreateNamespace("other")).NotTo(Succeed())
			})

			It("should fail to change to empty namespace", func() {
				br := broker.NewUserBroker(user, context.Background())
				Expect(br.CreateNamespace("")).NotTo(Succeed())
			})

			It("should fail with invalid namespace", func() {
				br := broker.NewUserBroker(user, context.Background())
				Expect(br.CreateNamespace("invalid-namespace")).NotTo(Succeed())
			})
		})
	})

	Describe("Remove", func() {
		Context("with fresh new user", func() {
			It("should success to remove namespace", func() {
				br := broker.NewUserBroker(user, context.Background())
				Expect(br.RemoveNamespace()).To(Succeed())
			})
		})

		Context("when namespace already exists", func() {
			BeforeEach(func() {
				br := broker.NewUserBroker(user, context.Background())
				Expect(br.CreateNamespace(NAMESPACE)).To(Succeed())
			})

			It("should success to remove the namespace", func() {
				br := broker.NewUserBroker(user, context.Background())
				Expect(br.RemoveNamespace()).To(Succeed())
			})
		})

		Context("when applications exist in the namespace", func() {
			BeforeEach(createTestApp)

			It("should fail to remove the namespace", func() {
				br := broker.NewUserBroker(user, context.Background())
				Expect(br.RemoveNamespace()).NotTo(Succeed())
			})
		})
	})
})

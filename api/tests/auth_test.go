package api_test

import (
	. "github.com/onsi/ginkgo"
	. "github.com/onsi/ginkgo/extensions/table"
	. "github.com/onsi/gomega"

	"golang.org/x/net/context"
)

var _ = Describe("Security", func() {
	var cli *TestClient
	var ctx = context.Background()

	BeforeEach(func() {
		cli = NewTestClientWithUser(false)
	})

	AfterEach(func() {
		cli.Close()
	})

	Describe("Authentication", func() {
		Context("with correct user name and password", func() {
			It("should success", func() {
				_, err := cli.Authenticate(ctx, TEST_USER, TEST_PASSWORD)
				Ω(err).ShouldNot(HaveOccurred())
			})
		})

		DescribeTable("with incorrect user name or password",
			func(username, password string) {
				_, err := cli.Authenticate(ctx, username, password)
				Ω(err).Should(HaveOccurred())
			},

			Entry("should fail with bad password", TEST_USER, "bad_password"),
			Entry("should fail if user not exist", "nonexist", TEST_PASSWORD),
			Entry("should fail with empty username", "", TEST_PASSWORD),
			Entry("should fail with empty password", TEST_USER, ""),
		)
	})

	Describe("Authorization", func() {
		Context("with login", func() {
			It("should success to access protected resource", func() {
				token, err := cli.Authenticate(ctx, TEST_USER, TEST_PASSWORD)
				Ω(err).ShouldNot(HaveOccurred())
				cli.SetToken(token)

				_, err = cli.GetApplications(ctx)
				Ω(err).ShouldNot(HaveOccurred())
			})
		})

		Context("without login", func() {
			It("should success to access unprotected resource", func() {
				_, err := cli.ServerVersion(ctx)
				Ω(err).ShouldNot(HaveOccurred())
			})

			It("should fail to access protected resource", func() {
				_, err := cli.GetApplications(ctx)
				Ω(err).Should(HaveOccurred())
			})
		})
	})
})

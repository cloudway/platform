package auth_test

import (
	"net/http"
	"os"
	"testing"

	. "github.com/onsi/ginkgo"
	. "github.com/onsi/gomega"

	"github.com/cloudway/platform/auth"
	"github.com/cloudway/platform/auth/userdb"
	_ "github.com/cloudway/platform/auth/userdb/mongodb"
)

func TestAuthenticator(t *testing.T) {
	os.Setenv("CLOUDWAY_USERDB_URL", "mongodb://127.0.0.1:27017/authz_test")

	RegisterFailHandler(Fail)
	RunSpecs(t, "Authenticator Suite")
}

var _ = Describe("Authenticator", func() {
	const (
		TEST_USER      = "test@example.com"
		TEST_NAMESPACE = "test"
		TEST_PASSWORD  = "test"
	)

	var (
		db    *userdb.UserDatabase
		authz *auth.Authenticator
	)

	BeforeEach(func() {
		var err error

		db, err = userdb.Open()
		Expect(err).NotTo(HaveOccurred())

		authz, err = auth.NewAuthenticator(db)
		Expect(err).NotTo(HaveOccurred())

		user := userdb.BasicUser{Name: TEST_USER, Namespace: TEST_NAMESPACE}
		err = db.Create(&user, TEST_PASSWORD)
		Expect(err).NotTo(HaveOccurred())
	})

	AfterEach(func() {
		db.Remove(TEST_USER)
		db.Close()
	})

	Describe("Authenticate", func() {
		It("should success with correct password", func() {
			user, _, err := authz.Authenticate(TEST_USER, TEST_PASSWORD)
			Expect(err).NotTo(HaveOccurred())
			Expect(user.Name).To(Equal(TEST_USER))
		})

		It("should fail with incorrect password", func() {
			_, _, err := authz.Authenticate(TEST_USER, "unknown")
			Expect(err).To(HaveOccurred())
		})

		It("should fail when user does not exist", func() {
			_, _, err := authz.Authenticate("nobody@example.com", "test")
			Expect(err).To(HaveOccurred())
		})
	})

	Describe("Verify", func() {
		It("should success with correct token", func() {
			var err error

			_, token, err := authz.Authenticate(TEST_USER, TEST_PASSWORD)
			Expect(err).NotTo(HaveOccurred())

			// create fake request
			r, err := http.NewRequest("GET", "/", nil)
			Expect(err).NotTo(HaveOccurred())

			r.Header.Set("Authorization", "bearer "+token)
			user, err := authz.Verify(r)
			Expect(err).NotTo(HaveOccurred())
			Expect(user.Name).To(Equal(TEST_USER))
			Expect(user.Namespace).To(Equal(TEST_NAMESPACE))
		})

		It("should fail with incorrect token", func() {
			r, err := http.NewRequest("GET", "/", nil)
			Expect(err).NotTo(HaveOccurred())

			r.Header.Set("Authorization", "bearer INVALID_TOKEN")
			_, err = authz.Verify(r)
			Expect(err).To(HaveOccurred())
		})

		It("should fail without Authorization header", func() {
			r, err := http.NewRequest("GET", "/", nil)
			Expect(err).NotTo(HaveOccurred())

			_, err = authz.Verify(r)
			Expect(err).To(HaveOccurred())
		})

		It("should fail with incorrect Authorization header", func() {
			r, err := http.NewRequest("GET", "/", nil)
			Expect(err).NotTo(HaveOccurred())

			r.Header.Set("Authorization", "basic INVALID_AUTH")
			_, err = authz.Verify(r)
			Expect(err).To(HaveOccurred())
		})
	})
})

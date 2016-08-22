package broker_test

import (
	"path/filepath"

	. "github.com/onsi/ginkgo"
	. "github.com/onsi/gomega"

	"github.com/cloudway/platform/auth/userdb"
	. "github.com/cloudway/platform/auth/userdb/matchers"
)

var _ = Describe("Users", func() {
	Describe("Create and remove user", func() {
		It("should behaviors normal", func() {
			var user userdb.BasicUser
			var path = filepath.Join(REPOROOT, NAMESPACE)

			newuser := userdb.BasicUser{
				Name:      TESTUSER,
				Namespace: NAMESPACE,
			}

			By("Initial state")

			Expect(broker.Users.Find(TESTUSER, &user)).To(BeUserNotFound(TESTUSER))
			Expect(path).NotTo(BeADirectory())

			By("Creating new user")

			Expect(broker.CreateUser(&newuser, "test")).To(Succeed())
			Expect(broker.Users.Find(TESTUSER, &user)).To(Succeed())
			Expect(user.Name).To(Equal(TESTUSER))
			Expect(user.Namespace).To(Equal(NAMESPACE))
			Expect(path).To(BeADirectory())

			By("Removing the user")

			Expect(broker.RemoveUser(TESTUSER)).To(Succeed())
			Expect(broker.Users.Find(TESTUSER, &user)).To(BeUserNotFound(TESTUSER))
			Expect(path).NotTo(BeADirectory())
		})
	})

	Describe("Refresh user broker", func() {
		type CustomUser struct {
			userdb.BasicUser `bson:",inline"`
			ExtraField       string
		}

		var user CustomUser

		BeforeEach(func() {
			user.Name = TESTUSER
			user.ExtraField = "test value"
			Expect(broker.CreateUser(&user, "test")).To(Succeed())
		})

		AfterEach(func() {
			broker.RemoveUser(TESTUSER)
		})

		It("should load fresh values from database", func() {
			br := broker.NewUserBroker(&user)
			Expect(br.User).To(BeIdenticalTo(&user))

			Expect(br.Users.Update(TESTUSER, userdb.Args{"extrafield": "new field value"})).To(Succeed())
			Expect(br.Refresh()).To(Succeed())

			newuser, ok := br.User.(*CustomUser)
			Expect(ok).To(BeTrue())
			Expect(newuser.ExtraField).To(Equal("new field value"))
		})

		It("should fail after removed user", func() {
			br := broker.NewUserBroker(&user)
			Expect(br.RemoveUser(TESTUSER)).To(Succeed())
			Expect(br.Refresh()).To(BeUserNotFound(TESTUSER))
		})
	})
})

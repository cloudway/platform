package broker_test

import (
	"path/filepath"

	. "github.com/onsi/ginkgo"
	. "github.com/onsi/gomega"

	"github.com/cloudway/platform/auth/userdb"
	. "github.com/cloudway/platform/auth/userdb/matchers"
)

var _ = Describe("Users", func() {
	It("should behaviors like", func() {
		var user userdb.BasicUser
		var path = filepath.Join(REPOROOT, NAMESPACE)

		newuser := userdb.BasicUser{
			Name:      USERNAME,
			Namespace: NAMESPACE,
		}

		By("Initial state")

		Expect(broker.Users.Find(USERNAME, &user)).To(BeUserNotFound(USERNAME))
		Expect(path).NotTo(BeADirectory())

		By("Creating new user")

		Expect(broker.CreateUser(&newuser, "test")).To(Succeed())
		Expect(broker.Users.Find(USERNAME, &user)).To(Succeed())
		Expect(user.Name).To(Equal(USERNAME))
		Expect(user.Namespace).To(Equal(NAMESPACE))
		Expect(path).To(BeADirectory())

		By("Remove the user")

		Expect(broker.RemoveUser(USERNAME)).To(Succeed())
		Expect(broker.Users.Find(USERNAME, &user)).To(BeUserNotFound(USERNAME))
		Expect(path).NotTo(BeADirectory())
	})
})

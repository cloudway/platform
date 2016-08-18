package userdb_test

import (
	"os"
	"testing"

	. "github.com/onsi/ginkgo"
	. "github.com/onsi/gomega"

	"github.com/cloudway/platform/auth/userdb"
	_ "github.com/cloudway/platform/auth/userdb/mongodb"
)

func TestUserDB(t *testing.T) {
	os.Setenv("CLOUDWAY_USERDB_URL", "mongodb://127.0.0.1:27017/userdb_test")

	RegisterFailHandler(Fail)
	RunSpecs(t, "UserDB Suite")
}

type CustomUser struct {
	userdb.BasicUser `bson:",inline"`

	StringField  string
	IntegerField int
	BoolField    bool
}

var _ = Describe("UserDB", func() {
	const (
		TEST_USER       = "test@example.com"
		TEST_NAMESPACE  = "test"
		OTHER_USER      = "other@example.com"
		OTHER_NAMESPACE = "other"
		NEW_USER        = "new@example.com"
		NEW_NAMESPACE   = "new"
		CUSTOM_USER     = "custom@example.com"
		NOSUCH_USER     = "nobody@example.com"
	)

	var db *userdb.UserDatabase

	BeforeEach(func() {
		var err error

		db, err = userdb.Open()
		Expect(err).NotTo(HaveOccurred())

		testUser := userdb.BasicUser{
			Name:      TEST_USER,
			Namespace: TEST_NAMESPACE,
		}

		otherUser := userdb.BasicUser{
			Name:      OTHER_USER,
			Namespace: OTHER_NAMESPACE,
		}

		customUser := &CustomUser{
			BasicUser: userdb.BasicUser{
				Name:      CUSTOM_USER,
				Namespace: "custom",
			},
			StringField:  "custom user",
			IntegerField: 42,
			BoolField:    true,
		}

		err = db.Create(&testUser, "test")
		Expect(err).NotTo(HaveOccurred())
		err = db.Create(&otherUser, "other")
		Expect(err).NotTo(HaveOccurred())
		err = db.Create(customUser, "custom")
		Expect(err).NotTo(HaveOccurred())
	})

	AfterEach(func() {
		db.Remove(TEST_USER)
		db.Remove(OTHER_USER)
		db.Remove(CUSTOM_USER)
		db.Close()
	})

	var checkUserNamespace = func(name, namespace string) {
		var user userdb.BasicUser
		err := db.Find(name, &user)
		Expect(err).NotTo(HaveOccurred())
		Expect(user.Namespace).To(Equal(namespace))
	}

	var checkCustomFields = func(user *CustomUser) {
		Expect(user.StringField).To(Equal("custom user"))
		Expect(user.IntegerField).To(Equal(42))
		Expect(user.BoolField).To(Equal(true))
	}

	Describe("Create user", func() {
		It("should fail with duplicate name", func() {
			user := userdb.BasicUser{
				Name: TEST_USER,
			}
			err := db.Create(&user, "test")
			Expect(err).To(HaveOccurred())
		})

		It("should fail with duplicate namespace", func() {
			user := userdb.BasicUser{
				Name:      NEW_USER,
				Namespace: TEST_NAMESPACE,
			}
			err := db.Create(&user, "test")
			Expect(err).To(HaveOccurred())
		})

		It("should fail with empty user name", func() {
			user := userdb.BasicUser{Name: ""}
			err := db.Create(&user, "test")
			Expect(err).To(HaveOccurred())
		})

		It("should fail with empty password", func() {
			user := userdb.BasicUser{Name: NEW_USER}
			err := db.Create(&user, "")
			Expect(err).To(HaveOccurred())
		})
	})

	Describe("Set namespace", func() {
		It("should success when set to a new namespace", func() {
			err := db.SetNamespace(TEST_USER, NEW_NAMESPACE)
			Expect(err).NotTo(HaveOccurred())
			checkUserNamespace(TEST_USER, NEW_NAMESPACE)
		})

		It("should success when set to an empty namespace", func() {
			err := db.SetNamespace(TEST_USER, "")
			Expect(err).NotTo(HaveOccurred())
			checkUserNamespace(TEST_USER, "")
		})

		It("should fail when set to an existing namespace", func() {
			err := db.SetNamespace(TEST_USER, OTHER_NAMESPACE)
			Expect(err).To(HaveOccurred())
			checkUserNamespace(TEST_USER, TEST_NAMESPACE)
		})
	})

	Describe("Find user", func() {
		It("should success if user exist", func() {
			var user userdb.BasicUser
			err := db.Find(TEST_USER, &user)
			Expect(err).NotTo(HaveOccurred())
			Expect(user.Name).To(Equal(TEST_USER))
			Expect(user.Namespace).To(Equal(TEST_NAMESPACE))
		})

		It("should fail if user does not exist", func() {
			var user userdb.BasicUser
			err := db.Find(NOSUCH_USER, &user)
			Expect(err).To(HaveOccurred())
		})
	})

	Describe("Authenticate", func() {
		It("should success with correct password", func() {
			user, err := db.Authenticate(TEST_USER, "test")
			Expect(err).NotTo(HaveOccurred())
			Expect(user.Name).To(Equal(TEST_USER))
			Expect(user.Namespace).To(Equal(TEST_NAMESPACE))
		})

		It("should fail with incorrect password", func() {
			_, err := db.Authenticate(TEST_USER, "guessed")
			Expect(err).To(HaveOccurred())
		})

		It("should fail if user does not exist", func() {
			_, err := db.Authenticate(NOSUCH_USER, "test")
			Expect(err).To(HaveOccurred())
		})
	})

	Describe("Change password", func() {
		It("should success with correct old password", func() {
			err := db.ChangePassword(TEST_USER, "test", "changed")
			Expect(err).NotTo(HaveOccurred())
		})

		It("should fail with incorrect old password", func() {
			err := db.ChangePassword(TEST_USER, "unknown", "changed")
			Expect(err).To(HaveOccurred())
		})

		It("should success to authenticate with new password", func() {
			db.ChangePassword(TEST_USER, "test", "changed")
			_, err := db.Authenticate(TEST_USER, "changed")
			Expect(err).NotTo(HaveOccurred())
		})

		It("should fail to authenticate with old password", func() {
			db.ChangePassword(TEST_USER, "test", "changed")
			_, err := db.Authenticate(TEST_USER, "test")
			Expect(err).To(HaveOccurred())
		})
	})

	Describe("Remove user", func() {
		It("should success if user exist", func() {
			err := db.Remove(TEST_USER)
			Expect(err).NotTo(HaveOccurred())
		})

		It("should fail if user does not exist", func() {
			err := db.Remove(NOSUCH_USER)
			Expect(err).To(HaveOccurred())
		})

		It("should no longer exist when removed", func() {
			db.Remove(TEST_USER)
			var user userdb.BasicUser
			err := db.Find(TEST_USER, &user)
			Expect(err).To(HaveOccurred())

		})

		It("should not authenticate with removed user", func() {
			db.Remove(TEST_USER)
			_, err := db.Authenticate(TEST_USER, "test")
			Expect(err).To(HaveOccurred())
		})
	})

	Describe("Custom user structure", func() {
		It("should persist custom field values", func() {
			var user CustomUser
			err := db.Find(CUSTOM_USER, &user)
			Expect(err).NotTo(HaveOccurred())
			checkCustomFields(&user)
		})

		It("should success to modify custom fields", func() {
			db.Update(CUSTOM_USER, userdb.Args{
				"stringfield":  "set to new value",
				"integerfield": 2016,
				"boolfield":    false,
			})

			var user CustomUser
			err := db.Find(CUSTOM_USER, &user)
			Expect(err).NotTo(HaveOccurred())
			Expect(user.StringField).To(Equal("set to new value"))
			Expect(user.IntegerField).To(Equal(2016))
			Expect(user.BoolField).To(Equal(false))
		})

		It("should success to load non-custom user but custom field values are zero", func() {
			var user CustomUser
			err := db.Find(TEST_USER, &user)
			Expect(err).NotTo(HaveOccurred())
			Expect(user.StringField).To(Equal(""))
			Expect(user.IntegerField).To(Equal(0))
			Expect(user.BoolField).To(Equal(false))
		})

		It("should act as a basic user", func() {
			var user userdb.BasicUser
			err := db.Find(CUSTOM_USER, &user)
			Expect(err).NotTo(HaveOccurred())
			Expect(user.Name).To(Equal(CUSTOM_USER))
		})

		It("can search with custom fields", func() {
			var user CustomUser
			err := db.Search(userdb.Args{"stringfield": "custom user"}, &user)
			Expect(err).NotTo(HaveOccurred())
			checkCustomFields(&user)
		})
	})
})

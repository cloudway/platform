package userdb_test

import (
	"os"
	"testing"

	. "github.com/onsi/ginkgo"
	. "github.com/onsi/gomega"
	"github.com/onsi/gomega/format"

	"github.com/cloudway/platform/auth/userdb"
	_ "github.com/cloudway/platform/auth/userdb/mongodb"
)

func TestUserDB(t *testing.T) {
	os.Setenv("CLOUDWAY_USERDB_URL", "mongodb://127.0.0.1:27017/userdb_test")

	RegisterFailHandler(Fail)
	RunSpecs(t, "UserDB Suite")
}

type BeDuplicateUser string

func (matcher BeDuplicateUser) Match(actual interface{}) (success bool, err error) {
	actualErr, ok := actual.(userdb.DuplicateUserError)
	return ok && string(actualErr) == string(matcher), nil
}

func (matcher BeDuplicateUser) FailureMessage(actual interface{}) (message string) {
	return format.Message(actual, "to be a", userdb.DuplicateUserError(string(matcher)))
}

func (matcher BeDuplicateUser) NegatedFailureMessage(actual interface{}) (message string) {
	return format.Message(actual, "not to be a", userdb.DuplicateUserError(string(matcher)))
}

type BeDuplicateNamespace string

func (matcher BeDuplicateNamespace) Match(actual interface{}) (success bool, err error) {
	actualErr, ok := actual.(userdb.DuplicateNamespaceError)
	return ok && string(actualErr) == string(matcher), nil
}

func (matcher BeDuplicateNamespace) FailureMessage(actual interface{}) (message string) {
	return format.Message(actual, "to be a", userdb.DuplicateNamespaceError(string(matcher)))
}

func (matcher BeDuplicateNamespace) NegatedFailureMessage(actual interface{}) (message string) {
	return format.Message(actual, "not to be a", userdb.DuplicateNamespaceError(string(matcher)))
}

type BeUserNotFound string

func (matcher BeUserNotFound) Match(actual interface{}) (success bool, err error) {
	actualErr, ok := actual.(userdb.UserNotFoundError)
	return ok && (string(matcher) == "" || string(matcher) == string(actualErr)), nil
}

func (matcher BeUserNotFound) FailureMessage(actual interface{}) (message string) {
	return format.Message(actual, "to be a", userdb.UserNotFoundError(string(matcher)))
}

func (matcher BeUserNotFound) NegatedFailureMessage(actual interface{}) (message string) {
	return format.Message(actual, "not to be a ", userdb.UserNotFoundError(string(matcher)))
}

var _ = Describe("UserDB", func() {
	const (
		TEST_USER       = "test@example.com"
		TEST_NAMESPACE  = "test"
		OTHER_USER      = "other@example.com"
		OTHER_NAMESPACE = "other"
		NEW_USER        = "new@example.com"
		NEW_NAMESPACE   = "new"
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

		Expect(db.Create(&testUser, "test")).To(Succeed())
		Expect(db.Create(&otherUser, "other")).To(Succeed())
	})

	AfterEach(func() {
		db.Remove(TEST_USER)
		db.Remove(OTHER_USER)
		db.Close()
	})

	var assertUserNamespace = func(name, namespace string) {
		var user userdb.BasicUser
		ExpectWithOffset(1, db.Find(name, &user)).To(Succeed())
		ExpectWithOffset(1, user.Namespace).To(Equal(namespace))
	}

	Describe("Create user", func() {
		It("should fail with duplicate name", func() {
			user := userdb.BasicUser{
				Name: TEST_USER,
			}
			Expect(db.Create(&user, "test")).To(BeDuplicateUser(TEST_USER))
		})

		It("should fail with duplicate namespace", func() {
			user := userdb.BasicUser{
				Name:      NEW_USER,
				Namespace: TEST_NAMESPACE,
			}
			Expect(db.Create(&user, "test")).To(BeDuplicateNamespace(TEST_NAMESPACE))
		})

		It("should fail with empty user name", func() {
			user := userdb.BasicUser{Name: ""}
			Expect(db.Create(&user, "test")).NotTo(Succeed())
		})

		It("should fail with empty password", func() {
			user := userdb.BasicUser{Name: NEW_USER}
			Expect(db.Create(&user, "")).NotTo(Succeed())
		})
	})

	Describe("Set namespace", func() {
		It("should success when set to a new namespace", func() {
			Expect(db.SetNamespace(TEST_USER, NEW_NAMESPACE)).To(Succeed())
			assertUserNamespace(TEST_USER, NEW_NAMESPACE)
		})

		It("should success when set to an empty namespace", func() {
			Expect(db.SetNamespace(TEST_USER, "")).To(Succeed())
			assertUserNamespace(TEST_USER, "")
		})

		It("should fail when set to an existing namespace", func() {
			Expect(db.SetNamespace(TEST_USER, OTHER_NAMESPACE)).To(BeDuplicateNamespace(OTHER_NAMESPACE))
			assertUserNamespace(TEST_USER, TEST_NAMESPACE)
		})
	})

	Describe("Find user", func() {
		It("should success if user exist", func() {
			var user userdb.BasicUser
			Expect(db.Find(TEST_USER, &user)).To(Succeed())
			Expect(user.Name).To(Equal(TEST_USER))
			Expect(user.Namespace).To(Equal(TEST_NAMESPACE))
		})

		It("should fail if user does not exist", func() {
			var user userdb.BasicUser
			Expect(db.Find(NOSUCH_USER, &user)).To(BeUserNotFound(NOSUCH_USER))
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
			Expect(err).To(BeUserNotFound(NOSUCH_USER))
		})
	})

	Describe("Change password", func() {
		It("should success with correct old password", func() {
			Expect(db.ChangePassword(TEST_USER, "test", "changed")).To(Succeed())
		})

		It("should fail with incorrect old password", func() {
			Expect(db.ChangePassword(TEST_USER, "unknown", "changed")).NotTo(Succeed())
		})

		It("should fail when user does not exist", func() {
			Expect(db.ChangePassword(NOSUCH_USER, "anything", "changed")).To(BeUserNotFound(NOSUCH_USER))
		})

		It("should success to authenticate with new password", func() {
			Expect(db.ChangePassword(TEST_USER, "test", "changed")).To(Succeed())
			_, err := db.Authenticate(TEST_USER, "changed")
			Expect(err).NotTo(HaveOccurred())
		})

		It("should fail to authenticate with old password", func() {
			Expect(db.ChangePassword(TEST_USER, "test", "changed")).To(Succeed())
			_, err := db.Authenticate(TEST_USER, "test")
			Expect(err).To(HaveOccurred())
		})
	})

	Describe("Remove user", func() {
		It("should success if user exist", func() {
			Expect(db.Remove(TEST_USER)).To(Succeed())
		})

		It("should fail if user does not exist", func() {
			Expect(db.Remove(NOSUCH_USER)).To(BeUserNotFound(NOSUCH_USER))
		})

		It("should no longer exist after removed", func() {
			var user userdb.BasicUser
			Expect(db.Remove(TEST_USER)).To(Succeed())
			Expect(db.Find(TEST_USER, &user)).To(BeUserNotFound(TEST_USER))
		})

		It("should not authenticate with removed user", func() {
			Expect(db.Remove(TEST_USER)).To(Succeed())
			_, err := db.Authenticate(TEST_USER, "test")
			Expect(err).To(BeUserNotFound(TEST_USER))
		})
	})

	Describe("Custom user", func() {
		type CustomUser struct {
			userdb.BasicUser `bson:",inline"`

			StringField  string
			IntegerField int
			BoolField    bool
		}

		const (
			CUSTOM_USER      = "custom@example.com"
			CUSTOM_NAMESPACE = "custom"
			CUSTOM_FIELD     = "custom user"
		)

		BeforeEach(func() {
			customUser := &CustomUser{
				BasicUser: userdb.BasicUser{
					Name:      CUSTOM_USER,
					Namespace: CUSTOM_NAMESPACE,
				},
				StringField:  CUSTOM_FIELD,
				IntegerField: 42,
				BoolField:    true,
			}

			Expect(db.Create(customUser, "custom")).To(Succeed())
		})

		AfterEach(func() {
			db.Remove(CUSTOM_USER)
		})

		var assertCustomFields = func(user *CustomUser) {
			ExpectWithOffset(1, user.StringField).To(Equal(CUSTOM_FIELD))
			ExpectWithOffset(1, user.IntegerField).To(Equal(42))
			ExpectWithOffset(1, user.BoolField).To(BeTrue())
		}

		It("should persist custom field values", func() {
			var user CustomUser
			Expect(db.Find(CUSTOM_USER, &user)).To(Succeed())
			assertCustomFields(&user)
		})

		It("should success to modify custom fields", func() {
			Expect(db.Update(CUSTOM_USER, userdb.Args{
				"stringfield":  "set to new value",
				"integerfield": 2016,
				"boolfield":    false,
			})).To(Succeed())

			var user CustomUser
			Expect(db.Find(CUSTOM_USER, &user)).To(Succeed())
			Expect(user.StringField).To(Equal("set to new value"))
			Expect(user.IntegerField).To(Equal(2016))
			Expect(user.BoolField).To(BeFalse())
		})

		It("should success to load non-custom user with custom field values set to zero", func() {
			var user CustomUser
			Expect(db.Find(TEST_USER, &user)).To(Succeed())
			Expect(user.StringField).To(BeZero())
			Expect(user.IntegerField).To(BeZero())
			Expect(user.BoolField).To(BeZero())
		})

		It("should act as a basic user", func() {
			var user userdb.BasicUser
			Expect(db.Find(CUSTOM_USER, &user)).To(Succeed())
			Expect(user.Name).To(Equal(CUSTOM_USER))
		})

		Context("search for user with custom fields", func() {
			It("should success if custom fields exists", func() {
				var user CustomUser
				Expect(db.Search(userdb.Args{"stringfield": "custom user"}, &user)).To(Succeed())
				assertCustomFields(&user)
			})

			It("should fail if custom fields does not exist", func() {
				var user CustomUser
				Expect(db.Search(userdb.Args{"stringfield": "no such user"}, &user)).To(BeUserNotFound(""))
			})
		})

		Context("search for collection of users with custom fields", func() {
			It("should success if custom fields exists", func() {
				var users []*CustomUser
				Expect(db.Search(userdb.Args{"stringfield": "custom user"}, &users)).To(Succeed())
				Expect(users).To(HaveLen(1))
				assertCustomFields(users[0])
			})

			It("should success if custom fields does not exist", func() {
				var users []*CustomUser
				Expect(db.Search(userdb.Args{"stringfield": "no such user"}, &users)).To(Succeed())
				Expect(users).To(BeEmpty())
			})
		})
	})
})

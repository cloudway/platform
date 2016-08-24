package matchers

import (
	"github.com/cloudway/platform/auth/userdb"
	"github.com/onsi/gomega/format"
)

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

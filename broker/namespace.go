package broker

import (
	"errors"
	"regexp"
)

var namespacePattern = regexp.MustCompile("^[a-z][a-z_0-9]*$")

func (br *UserBroker) CreateNamespace(namespace string) (err error) {
	if namespace == "" {
		return errors.New("The namespace cannot be empty")
	}
	if !namespacePattern.MatchString(namespace) {
		return errors.New("The namespace can only contains lower case letters, digits, or underscores")
	}

	if err = br.Refresh(); err != nil {
		return err
	}

	user := br.User.Basic()
	oldNamespace := user.Namespace

	if namespace == oldNamespace {
		return nil
	}

	// make sure no applications exists in the old namespace
	if len(user.Applications) != 0 {
		return NamespaceNotEmptyError(user.Namespace)
	}

	// update the namespace in the user database,
	// may conflict if namespace already exists
	err = br.Users.SetNamespace(user.Name, namespace)
	if err != nil {
		return err
	}

	// recreate namespace in the SCM
	if oldNamespace != "" {
		err = br.SCM.RemoveNamespace(oldNamespace)
	}
	if err == nil {
		err = br.SCM.CreateNamespace(namespace)
	}

	// restore user database if failed to recreate SCM namespace
	if err != nil {
		br.Users.SetNamespace(user.Name, oldNamespace)
		return err
	}

	user.Namespace = namespace
	return nil
}

func (br *UserBroker) RemoveNamespace() (err error) {
	if err = br.Refresh(); err != nil {
		return err
	}
	user := br.User.Basic()

	if user.Namespace == "" {
		return nil
	}

	if len(user.Applications) != 0 {
		return NamespaceNotEmptyError(user.Namespace)
	}

	err = br.SCM.RemoveNamespace(user.Namespace)
	if err != nil {
		return err
	}

	err = br.Users.SetNamespace(user.Name, "")
	if err != nil {
		return err
	}

	user.Namespace = ""
	return nil
}

package broker

func (br *UserBroker) CreateNamespace(namespace string) (err error) {
	user := br.User.Basic()

	if user.Namespace == namespace {
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
	if user.Namespace != "" {
		br.SCM.RemoveNamespace(user.Namespace)
	}

	user.Namespace = namespace
	return br.SCM.CreateNamespace(namespace)
}

func (br *UserBroker) RemoveNamespace(force bool) (err error) {
	user := br.User.Basic()

	if user.Namespace == "" {
		return nil
	}

	if !force && len(user.Applications) != 0 {
		return NamespaceNotEmptyError(user.Namespace)
	}

	err = br.Users.SetNamespace(user.Name, "")
	if err != nil {
		return err
	}

	err = br.SCM.RemoveNamespace(user.Namespace)
	if err != nil {
		return err
	}

	user.Namespace = ""
	return nil
}

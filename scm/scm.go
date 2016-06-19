package scm

import "errors"

// Source Code Management interface.
type SCM interface {
    // Create a namespace in the SCM.
    // For user based SCM, a new user is created.
    // For project based SCM, a new project is created.
    // Otherwise, the SCM must have a separate database to manage namespace and repositories.
    CreateNamespace(namespace string) error

    // Remove the namespace from SCM. All repositories in the namespace
    // are also removed.
    RemoveNamespace(namespace string) error

    // Create a new repository with the given name in the given namespace.
    CreateRepo(namespace, name string) error

    // Remove the repository with the given name in the given namespace.
    RemoveRepo(namespace, name string) error

    // Add an SSH key to the given namespace.
    AddKey(namespace string, key string) error

    // Remove an SSH key from the given namespace.
    RemoveKey(namespace string, key string) error

    // List all SSH keys in the given namespace.
    ListKeys(namespace string) ([]string, error)
}

var New = func() (SCM, error) {
    return nil, errors.New("The SCM plugin does not configured")
}

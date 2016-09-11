package scm

import (
	"fmt"
	"io"

	"github.com/cloudway/platform/config"
)

// Source Code Management interface.
type SCM interface {
	// Type returns the SCM type, such as "git", "hg", etc.
	Type() string

	// Create a namespace in the SCM.
	// For user based SCM, a new user is created.
	// For project based SCM, a new project is created.
	// Otherwise, the SCM must have maintain a database to manage namespaces
	// and repositories.
	CreateNamespace(namespace string) error

	// Remove the namespace from SCM. All repositories in the namespace
	// are also removed.
	RemoveNamespace(namespace string) error

	// Create a new repository with the given name in the given namespace.
	CreateRepo(namespace, name string, purge bool) error

	// Remove the repository with the given name in the given namespace.
	RemoveRepo(namespace, name string) error

	// Populate repository from a template.
	Populate(namespace, name string, payload io.Reader, size int64) error

	// Populate repository from an URL.
	PopulateURL(namespace, name string, url string) error

	// Deploy application with new commit. Log build output to the give writer.
	Deploy(namespace, name string, branch string, stdout, stderr io.Writer) error

	// Get the current deployment branch.
	GetDeploymentBranch(namespace, name string) (*Branch, error)

	// Get all deployment branches.
	GetDeploymentBranches(namespace, name string) ([]*Branch, error)

	// Add an SSH key to the given namespace.
	AddKey(namespace string, key string) error

	// Remove an SSH key from the given namespace.
	RemoveKey(namespace string, key string) error

	// List all SSH keys in the given namespace.
	ListKeys(namespace string) ([]SSHKey, error)
}

// A branch of deployment.
type Branch struct {
	// The branch identifier, this is a ref-id for git SCM.
	Id string `json:"id"`

	// The display identifier.
	DisplayId string `json:"displayId,omitempty"`

	// The branch type, such as "BRANCH" or "TAG"
	Type string `json:"type,omitempty"`
}

type SSHKey struct {
	Label string
	Text  string
}

var New = func() (SCM, error) {
	scmtype := config.Get("scm.type")
	if scmtype == "" {
		return nil, fmt.Errorf("The SCM plugin does not configured")
	}
	return nil, fmt.Errorf("Unsupported SCM type: %s", scmtype)
}

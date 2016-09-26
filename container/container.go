package container

import (
	"context"
	"fmt"
	"io"
	"regexp"
	"strings"

	"github.com/cloudway/platform/pkg/manifest"
	"github.com/cloudway/platform/pkg/serverlog"
)

// Engine is an abstract interface to the underlying containerization
// infrastructure.
type Engine interface {
	// ServerVersion returns the engine server version.
	ServerVersion(ctx context.Context) (string, error)

	// Create create a new application container.
	Create(ctx context.Context, opts CreateOptions) ([]Container, error)

	// Inspect returns an application container constructed from the
	// container id in the system.
	Inspect(ctx context.Context, id string) (Container, error)

	// FindInNamespace finds all containers in the given namespace.
	// If the namespace is an empty string, then returns all containers
	// in the system.
	FindInNamespace(ctx context.Context, namespace string) ([]Container, error)

	// FindAll finds all containers with the given name and namespace.
	FindAll(ctx context.Context, name, namespace string) ([]Container, error)

	// FindApplications finds all application containers with the given name
	// and namespace.
	FindApplications(ctx context.Context, name, namespace string) ([]Container, error)

	// FindService find a service container with the given name, namespace
	// and service name.
	FindService(ctx context.Context, name, namespace, service string) ([]Container, error)

	// DistributeRepo distribute repository to containers.
	DistributeRepo(ctx context.Context, containers []Container, repo io.Reader, zip bool) error

	// DeployRepo deploy repository to containers.
	DeployRepo(ctx context.Context, name, namespace string, in io.Reader, log *serverlog.ServerLog) error
}

// Container is an abstract interface to the underlying container.
type Container interface {
	Engine
	Info

	// Start the application container.
	Start(ctx context.Context, log *serverlog.ServerLog) error

	// Restart the application container.
	Restart(ctx context.Context, log *serverlog.ServerLog) error

	// Stop the application container.
	Stop(ctx context.Context) error

	// Destroy destroys the container.
	Destroy(ctx context.Context) error

	// Exec execute command in application container.
	Exec(ctx context.Context, user string, stdin io.Reader, stdout, stderr io.Writer, cmd ...string) error

	// ExecE execute the command and accumulate error messages from
	// standard error of the command.
	ExecE(ctx context.Context, user string, in io.Reader, out io.Writer, cmd ...string) error

	// ExecQ silently execute the command and accumulate error messages
	// from standard error of the command.
	ExecQ(ctx context.Context, user string, cmd ...string) error

	// Subst performs the expansion by executing command return the
	// contents as the standard output of the command, with any trailing
	// newlines deleted.
	Subst(ctx context.Context, user string, in io.Reader, cmd ...string) (string, error)

	// Processes returns running processes in the container.
	Processes(ctx context.Context) (*ProcessList, error)

	// Stats returns stream of statistics of a container.
	//
	// Note: The current API returns a stream of docker stats type encoded
	// in JSON data. Other implementations must returns the same data type
	// as defined by docker API.
	Stats(ctx context.Context, stream bool) (io.ReadCloser, error)

	// CopyTo copy files into container.
	CopyTo(ctx context.Context, path string, content io.Reader) error

	// CopyFrom copy files from container.
	CopyFrom(ctx context.Context, path string) (io.ReadCloser, error)

	// Deploy the application.
	Deploy(ctx context.Context, path string) error

	// GetInfo get application information from container.
	GetInfo(ctx context.Context, options ...string) (*manifest.SandboxInfo, error)

	// Setenv adds the variable to the environment with the value, if name
	// does not exists. If name does exist in the environment, then its value
	// is replaced by value.
	Setenv(ctx context.Context, name, value string) error

	// Getenv returns an environment variable value.
	Getenv(ctx context.Context, name string) (string, error)

	// ActiveState returns container active state.
	ActiveState(ctx context.Context) manifest.ActiveState

	// AddHost add more or more custom host to the container.
	AddHost(ctx context.Context, host string, more ...string) error

	// RemoveHost remove one or more custom host from the container.
	RemoveHost(ctx context.Context, host string, more ...string) error

	// GetHosts returns all custom host in the container.
	GetHosts(ctx context.Context) []string
}

// Info contains container informations.
type Info interface {
	ID() string
	Name() string
	Namespace() string
	Version() string
	Category() manifest.Category
	PluginTag() string
	Flags() uint32
	ServiceName() string
	DependsOn() []string
	Hostname() string
	FQDN() string
	IP() string
	User() string
	Home() string
	EnvDir() string
	RepoDir() string
	DeployDir() string
	DataDir() string
	LogDir() string
	StartedAt() string
}

// CreateOptions contains options when creating container.
type CreateOptions struct {
	Name        string
	Namespace   string
	ServiceName string
	Plugin      *manifest.Plugin
	Image       string
	Flags       uint32
	Secret      string
	Home        string
	User        string
	Network     string
	Capacity    string
	Scaling     int
	Hosts       []string
	Env         map[string]string
	Repo        string
	Log         *serverlog.ServerLog
}

// ProcessList contains running process list in a container.
type ProcessList struct {
	Processes [][]string
	Headers   []string
}

// StatusError reports an unsuccessful exit by a command
type StatusError struct {
	Command []string
	Code    int
	Message string
}

func (e StatusError) Error() string {
	if e.Message != "" {
		return e.Message
	} else {
		return fmt.Sprintf("exec command '%s' failed, Code: %d", strings.Join(e.Command, " "), e.Code)
	}
}

var reNamePattern = regexp.MustCompile(`^((\*|[a-z][a-z_0-9]*)\.)?([a-z][a-z_0-9]*)-([a-z][a-z_0-9]*)$`)

// SplitNames is a utility function that split a container specification
// into name, namespace, and service name.
func SplitNames(name string) (string, string, string) {
	m := reNamePattern.FindStringSubmatch(name)
	if len(m) != 0 {
		return m[2], m[3], m[4]
	} else {
		return "", "", ""
	}
}

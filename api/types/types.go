package types

import (
	"time"

	"github.com/cloudway/platform/pkg/manifest"
)

// Version information contains response of remote API:
// GET "/version"
type Version struct {
	Version       string
	GitCommit     string
	BuildTime     string
	DockerVersion string
	Os            string
	Arch          string
}

// ApplicationInfo contains response of remote API:
// GET "/applications/{name}"
type ApplicationInfo struct {
	Name      string
	Namespace string
	CreatedAt time.Time
	URL       string
	SCMType   string
	CloneURL  string
	SSHURL    string
	Framework *manifest.Plugin
	Services  []*manifest.Plugin
	Scaling   int
}

// CreateApplication struct contains post options of remote API:
// POST "/applications/"
type CreateApplication struct {
	Name      string
	Framework string
	Services  []string
	Repo      string
}

// Branch is a branch of deployment.
type Branch struct {
	// The branch identifier.
	Id string

	// The display identifier.
	DisplayId string

	// The branch type, such as "BRANCH" or "TAG"
	Type string
}

// Deployments contains response of remote API:
// GET "/applications/{name}/deploy"
type Deployments struct {
	// The current deployment branch
	Current *Branch

	// All deployment branches
	Branches []Branch
}

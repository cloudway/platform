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

// ContainerJSONBase identifies a container.
type ContainerJSONBase struct {
	ID          string
	Category    manifest.Category
	Name        string
	DisplayName string
}

// ContainerStatus contains response of remote API:
// Get "/applications/{name}/status"
type ContainerStatus struct {
	ContainerJSONBase
	IPAddress string
	Ports     []string
	State     manifest.ActiveState
}

// ProcessList contains response of remote API:
// Get "/applications/{name}/procs"
type ProcessList struct {
	ContainerJSONBase
	Headers   []string
	Processes [][]string
}

// ContainerStats contains response of remote API:
// Get "/applications/{name}/stats"
type ContainerStats struct {
	ID               string
	Name             string
	CPUTotalUsage    uint64
	CPUSystemUsage   uint64
	CPUPercentage    float64
	MemoryUsage      uint64
	MemoryLimit      uint64
	MemoryPercentage float64
	NetworkRx        uint64
	NetworkTx        uint64
	BlockRead        uint64
	BlockWrite       uint64
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
	Branches []*Branch
}

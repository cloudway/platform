package manifest

import "time"

type ApplicationInfo struct {
    Name        string
    Namespace   string
    CreatedAt   time.Time
    URL         string
    CloneURL    string
    SSHURL      string
    Framework   *Plugin
    Services    []*Plugin
}

type CreateApplication struct {
    Name        string
    Framework   string
    Services    []string
    Repo        string
}

// A branch of deployment.
type DeploymentBranch struct {
    // The branch identifier.
    Id string

    // The display identifier.
    DisplayId string

    // The branch type, such as "BRANCH" or "TAG"
    Type string
}

// Information about an application deployments.
type ApplicationDeployments struct {
    // The current deployment branch
    Current *DeploymentBranch

    // All deployment branches
    Branches []DeploymentBranch
}

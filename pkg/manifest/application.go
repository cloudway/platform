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

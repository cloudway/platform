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

package manifest

type SandboxInfo struct {
    Env             map[string]string
    Endpoints       []*Endpoint
    Plugins         []*Plugin
    State           ActiveState
}

type ActiveState byte

const (
    StateNew ActiveState = iota
    StateStarting
    StateRestarting
    StateRunning
    StateStopping
    StateStopped
    StateBuilding
    StateFailed
    StateUnknown
)

var stateString = [...]string{
    StateNew:        "new",
    StateStarting:   "starting",
    StateRestarting: "restarting",
    StateRunning:    "running",
    StateStopping:   "stopping",
    StateStopped:    "stopped",
    StateBuilding:   "building",
    StateFailed:     "failed",
    StateUnknown:    "unknown",
}

func (s ActiveState) String() string {
    return stateString[s]
}

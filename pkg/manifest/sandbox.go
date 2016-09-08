package manifest

type SandboxInfo struct {
	Env       map[string]string `json:"env,omitempty"`
	Endpoints []*Endpoint       `json:"endpoints,omitempty"`
	Plugins   []*Plugin         `json:"plugins,omitempty"`
	State     ActiveState       `json:"state,omitempty"`
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

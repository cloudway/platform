package container

import "github.com/docker/engine-api/types"

const notStarted = "0001-01-01T00:00:00Z"

type ContainerState struct {
    *types.ContainerState
}

func (s ContainerState) String() string {
    switch {
    case s.Paused:
        return "PAUSED"
    case s.Running:
        return "STARTED"
    default:
        if s.StartedAt == notStarted {
            return "NEW"
        } else {
            return "STOPPED"
        }
    }
}

package container

import "github.com/docker/engine-api/types"

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
        return "STOPPED"
    }
}
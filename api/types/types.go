package types

// Version information contains response of remote API:
// GET "/version"
type Version struct {
    Version         string
    DockerVersion   string
    Os              string
    Arch            string
}

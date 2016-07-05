package types

// Version information contains response of remote API:
// GET "/version"
type Version struct {
    Version         string
    GitCommit       string
    BuildTime       string
    DockerVersion   string
    Os              string
    Arch            string
}

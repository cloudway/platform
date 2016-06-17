package runtime

import (
    "github.com/cloudway/platform/container"
    "github.com/cloudway/platform/auth"
    "github.com/cloudway/platform/auth/user"
)

// Runtime mantains all external services used by API server.
type Runtime struct {
    container.DockerClient
    UserDB  *user.UserDatabase
    Authz   *auth.Authenticator
}

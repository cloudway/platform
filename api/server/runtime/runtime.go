package runtime

import (
    "github.com/cloudway/platform/container"
    "github.com/cloudway/platform/auth"
    "github.com/cloudway/platform/auth/userdb"
    "github.com/cloudway/platform/scm"
    "github.com/cloudway/platform/hub"

    // Load all plugings
    _ "github.com/cloudway/platform/auth/userdb/mongodb"
    _ "github.com/cloudway/platform/scm/bitbucket"
)

// Runtime mantains all external services used by API server.
type Runtime struct {
    container.DockerClient
    Users   *userdb.UserDatabase
    Authz   *auth.Authenticator
    SCM     scm.SCM
    Hub     *hub.PluginHub
}

func New(cli container.DockerClient) (rt *Runtime, err error) {
    rt = new(Runtime)
    rt.DockerClient = cli

    rt.Users, err = userdb.Open()
    if err != nil {
        return
    }

    rt.Authz, err = auth.NewAuthenticator(rt.Users)
    if err != nil {
        return
    }

    rt.SCM, err = scm.New()
    if err != nil {
        return
    }

    rt.Hub, err = hub.New()
    if err != nil {
        return
    }

    return rt, nil
}

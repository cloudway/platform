package client

import (
    "encoding/json"
    "golang.org/x/net/context"
    "github.com/cloudway/platform/api/types"
)

func (api *APIClient) ServerVersion(ctx context.Context) (types.Version, error) {
    var server types.Version
    resp, err := api.cli.Get(ctx, "/version", nil, nil)
    if err == nil {
        err = json.NewDecoder(resp.Body).Decode(&server)
        resp.EnsureClosed()
    }
    return server, err
}

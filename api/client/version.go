package client

import (
	"context"
	"encoding/json"

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

func (cli *APIClient) ClientVersion() string {
	return cli.cli.ClientVersion()
}

func (cli *APIClient) UpdateClientVersion(v string) {
	cli.cli.UpdateClientVersion(v)
}

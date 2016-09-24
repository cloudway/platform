package client

import (
	"context"
	"encoding/json"
	"net/url"
)

func (api *APIClient) GetNamespace(ctx context.Context) (namespace string, err error) {
	resp, err := api.cli.Get(ctx, "/namespace", nil, nil)
	if err == nil {
		var nsJson map[string]string
		err = json.NewDecoder(resp.Body).Decode(&nsJson)
		resp.EnsureClosed()
		namespace = nsJson["Namespace"]
	}
	return namespace, err
}

func (api *APIClient) SetNamespace(ctx context.Context, namespace string) error {
	query := url.Values{}
	query.Set("namespace", namespace)

	resp, err := api.cli.Post(ctx, "/namespace", query, nil, nil)
	resp.EnsureClosed()
	return err
}

func (api *APIClient) RemoveNamespace(ctx context.Context, force bool) error {
	var query url.Values
	if force {
		query = url.Values{}
		query.Set("force", "1")
	}

	resp, err := api.cli.Delete(ctx, "/namespace", query, nil)
	resp.EnsureClosed()
	return err
}

package client

import (
	"encoding/json"
	"io"
	"net/url"

	"github.com/cloudway/platform/pkg/manifest"
	"golang.org/x/net/context"
)

func (api *APIClient) GetInstalledPlugins(ctx context.Context, category manifest.Category) ([]*manifest.Plugin, error) {
	return api.listPlugins(ctx, category, false)
}

func (api *APIClient) GetUserPlugins(ctx context.Context, category manifest.Category) ([]*manifest.Plugin, error) {
	return api.listPlugins(ctx, category, true)
}

func (api *APIClient) listPlugins(ctx context.Context, category manifest.Category, userDefined bool) ([]*manifest.Plugin, error) {
	query := url.Values{}
	query.Set("category", string(category))
	if userDefined {
		query.Set("user", "1")
	}

	resp, err := api.cli.Get(ctx, "/plugins/", query, nil)
	if err != nil {
		return nil, err
	}

	var plugins []*manifest.Plugin
	err = json.NewDecoder(resp.Body).Decode(&plugins)
	resp.EnsureClosed()
	return plugins, err
}

func (api *APIClient) GetPluginInfo(ctx context.Context, tag string) (*manifest.Plugin, error) {
	resp, err := api.cli.Get(ctx, "/plugins/"+tag, nil, nil)
	if err != nil {
		return nil, err
	}

	var plugin *manifest.Plugin
	err = json.NewDecoder(resp.Body).Decode(&plugin)
	resp.EnsureClosed()
	return plugin, err
}

func (api *APIClient) InstallPlugin(ctx context.Context, body io.Reader) error {
	headers := map[string][]string{"Content-Type": {"application/tar"}}
	resp, err := api.cli.PostRaw(ctx, "/plugins/", nil, body, headers)
	resp.EnsureClosed()
	return err
}

func (api *APIClient) RemovePlugin(ctx context.Context, tag string) error {
	resp, err := api.cli.Delete(ctx, "/plugins/"+tag, nil, nil)
	resp.EnsureClosed()
	return err
}

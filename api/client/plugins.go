package client

import (
	"encoding/json"
	"fmt"
	"net/url"
	"strings"

	"github.com/cloudway/platform/pkg/manifest"
	"golang.org/x/net/context"
)

func (api *APIClient) ListPlugins(ctx context.Context, namespace string, category manifest.Category) ([]*manifest.Plugin, error) {
	path := "/plugins/"
	if namespace != "" {
		path += namespace + "/"
	}

	query := url.Values{}
	query.Set("category", string(category))

	resp, err := api.cli.Get(ctx, path, query, nil)
	if err != nil {
		return nil, err
	}

	var plugins []*manifest.Plugin
	err = json.NewDecoder(resp.Body).Decode(&plugins)
	resp.EnsureClosed()
	return plugins, err
}

func (api *APIClient) GetPluginInfo(ctx context.Context, tag string) (*manifest.Plugin, error) {
	if strings.HasSuffix(tag, "/") {
		return nil, fmt.Errorf("%s: Invalid plugin tag", tag)
	}

	resp, err := api.cli.Get(ctx, "/plugins/"+tag, nil, nil)
	if err != nil {
		return nil, err
	}

	var plugins []*manifest.Plugin
	err = json.NewDecoder(resp.Body).Decode(&plugins)
	resp.EnsureClosed()
	if err != nil {
		return nil, err
	}
	return plugins[0], err
}

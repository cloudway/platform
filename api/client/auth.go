package client

import (
	"encoding/base64"
	"encoding/json"
	"golang.org/x/net/context"
)

func (api *APIClient) Authenticate(ctx context.Context, username, password string) (token string, err error) {
	auth := string(base64.StdEncoding.EncodeToString([]byte(username + ":" + password)))
	headers := map[string][]string{"Authorization": {"Basic " + auth}}
	resp, err := api.cli.Post(ctx, "/auth", nil, nil, headers)
	if err == nil {
		var tokenJson map[string]string
		err = json.NewDecoder(resp.Body).Decode(&tokenJson)
		resp.EnsureClosed()
		token = tokenJson["Token"]
	}
	return token, err
}

func (api *APIClient) SetToken(token string) {
	if token != "" {
		api.cli.AddCustomHeader("Authorization", "Bearer "+token)
	} else {
		api.cli.RemoveCustomHeader("Authorization")
	}
}

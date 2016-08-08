package client

import (
    "encoding/json"
    "encoding/base64"
    "golang.org/x/net/context"
)

func (api *APIClient) Authenticate(ctx context.Context, username, password string) (token string, err error) {
    auth := string(base64.StdEncoding.EncodeToString([]byte(username+":"+password)))
    headers := map[string][]string{"Authorization": []string{"Basic " + auth}}
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
    api.cli.AddCustomHeader("Authorization", "Bearer "+token)
}

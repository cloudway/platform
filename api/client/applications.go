package client

import (
    "net/url"
    "encoding/json"
    "golang.org/x/net/context"
    "github.com/cloudway/platform/api/types"
)

func (api *APIClient) GetApplications(ctx context.Context) ([]string, error) {
    var apps []string
    resp, err := api.cli.Get(ctx, "/applications/", nil, nil)
    if err == nil {
        err = json.NewDecoder(resp.Body).Decode(&apps)
        resp.EnsureClosed()
    }
    return apps, err
}

func (api *APIClient) GetApplicationInfo(ctx context.Context, name string) (*types.ApplicationInfo, error) {
    var info types.ApplicationInfo
    resp, err := api.cli.Get(ctx, "/applications/"+name+"/info", nil, nil)
    if err == nil {
        err = json.NewDecoder(resp.Body).Decode(&info)
        resp.EnsureClosed()
    }
    return &info, err
}

func (api *APIClient) CreateApplication(ctx context.Context, opts types.CreateApplication) error {
    resp, err := api.cli.Post(ctx, "/applications/", nil, &opts, nil)
    resp.EnsureClosed()
    return err
}

func (api *APIClient) RemoveApplication(ctx context.Context, name string) error {
    resp, err := api.cli.Delete(ctx, "/applications/"+name, nil, nil)
    resp.EnsureClosed()
    return err
}

func (api *APIClient) StartApplication(ctx context.Context, name string) error {
    resp, err := api.cli.Post(ctx, "/applications/"+name+"/start", nil, nil, nil)
    resp.EnsureClosed()
    return err
}

func (api *APIClient) StopApplication(ctx context.Context, name string) error {
    resp, err := api.cli.Post(ctx, "/applications/"+name+"/stop", nil, nil, nil)
    resp.EnsureClosed()
    return err
}

func (api *APIClient) RestartApplication(ctx context.Context, name string) error {
    resp, err := api.cli.Post(ctx, "/applications/"+name+"/restart", nil, nil, nil)
    resp.EnsureClosed()
    return err
}

func (api *APIClient) DeployApplication(ctx context.Context, name, branch string) error {
    var query url.Values
    if branch != "" {
        query = url.Values{"branch": []string{branch}}
    }

    resp, err := api.cli.Post(ctx, "/applications/"+name+"/deploy", query, nil, nil)
    resp.EnsureClosed()
    return err
}

func (api *APIClient) GetApplicationDeployments(ctx context.Context, name string) (*types.Deployments, error) {
    var deployments types.Deployments
    resp, err := api.cli.Get(ctx, "/applications/"+name+"/deploy", nil, nil)
    if err == nil {
        err = json.NewDecoder(resp.Body).Decode(&deployments)
        resp.EnsureClosed()
    }
    return &deployments, err
}

func (api *APIClient) ScaleApplication(ctx context.Context, name, scaling string) error {
    query := url.Values{"scale": []string{scaling}}
    resp, err := api.cli.Post(ctx, "/applications/"+name+"/scale", query, nil, nil)
    resp.EnsureClosed()
    return err
}

func envpath(name, service string) string {
    if service == "" {
        service = "_"
    }
    return "/applications/"+name+"/services/"+service+"/env/"
}

func (api *APIClient) ApplicationEnviron(ctx context.Context, name, service string) (map[string]string, error) {
    var env map[string]string
    resp, err := api.cli.Get(ctx, envpath(name, service), nil, nil)
    if err == nil {
        err = json.NewDecoder(resp.Body).Decode(&env)
        resp.EnsureClosed()
    }
    return env, err
}

func (api *APIClient) ApplicationGetenv(ctx context.Context, name, service, key string) (string, error) {
    var env map[string]string
    resp, err := api.cli.Get(ctx, envpath(name, service) + key, nil, nil)
    if err == nil {
        err = json.NewDecoder(resp.Body).Decode(&env)
        resp.EnsureClosed()
    }
    return env[key], err
}

func (api *APIClient) ApplicationSetenv(ctx context.Context, name, service string, env map[string]string) error {
    resp, err := api.cli.Post(ctx, envpath(name, service), nil, env, nil)
    resp.EnsureClosed()
    return err
}

func (api *APIClient) ApplicationUnsetenv(ctx context.Context, name, service string, keys ...string) error {
    env := make(map[string]string)
    for _, k := range keys {
        env[k] = ""
    }

    query := url.Values{"remove": []string{""}}
    resp, err := api.cli.Post(ctx, envpath(name, service), query, env, nil)
    resp.EnsureClosed()
    return err
}

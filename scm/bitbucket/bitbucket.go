package bitbucket

import (
    "fmt"
    "errors"
    "strconv"
    "strings"
    "encoding/base64"
    "encoding/json"
    "net/url"
    "net/http"
    "golang.org/x/net/context"
    "github.com/cloudway/platform/pkg/rest"
    "github.com/cloudway/platform/scm"
    "github.com/cloudway/platform/container/conf"
)

type bitbucketClient struct {
    *rest.Client
}

func init() {
    old := scm.New
    scm.New = func() (scm.SCM, error) {
        scmtype := conf.Get("scm.type")
        if scmtype != "bitbucket" {
            return old()
        }

        scmurl := conf.Get("scm.url")
        if scmurl == "" {
            return nil, errors.New("Bitbucket URL not configured")
        }
        u, err := url.Parse(scmurl)
        if err != nil {
            return nil, err
        }

        username := u.User.Username()
        password, _ := u.User.Password()
        auth := string(base64.StdEncoding.EncodeToString([]byte(username + ":" + password)))

        headers := map[string]string {
            "Authorization" : "Basic " + auth, // TODO
        }

        cli, err := rest.NewClient(scmurl, "", nil, headers)
        if err != nil {
            return nil, err
        }
        return &bitbucketClient{cli}, nil
    }
}

func (cli *bitbucketClient) CreateNamespace(namespace string) error {
    opts := CreateProjectOpts{
        Key:  namespace,
        Name: namespace,
    }

    path := "/rest/api/1.0/projects"
    resp, err := cli.Post(context.Background(), path, nil, opts, nil)
    return checkNamespaceError(namespace, resp.StatusCode, err)
}

func (cli *bitbucketClient) RemoveNamespace(namespace string) error {
    ctx := context.Background()
    start := 0

    for {
        path := fmt.Sprintf("/rest/api/1.0/projects/%s/repos", namespace)
        params := url.Values{"start": []string{strconv.Itoa(start)}}
        resp, err := cli.Get(ctx, path, params, nil)
        err = checkNamespaceError(namespace, resp.StatusCode, err)
        if err != nil {
            return err
        }

        var page RepoPage
        err = json.NewDecoder(resp.Body).Decode(&page)
        if err != nil {
            return err
        }

        for _, repo := range page.Values {
            path := fmt.Sprintf("/rest/api/1.0/projects/%s/repos/%s", namespace, repo.Slug)
            resp, err := cli.Delete(ctx, path, nil, nil)
            if err != nil && resp.StatusCode != http.StatusNotFound {
                return err
            }
        }

        if page.IsLastPage {
            break
        }
        start = page.NextPageStart
    }

    path := fmt.Sprintf("/rest/api/1.0/projects/%s", namespace)
    resp, err := cli.Delete(context.Background(), path, nil, nil)
    return checkNamespaceError(namespace, resp.StatusCode, err)
}

func (cli *bitbucketClient) CreateRepo(namespace, name string) error {
    opts := CreateRepoOpts{
        Name: name,
    }

    path := fmt.Sprintf("/rest/api/1.0/projects/%s/repos", namespace)
    resp, err := cli.Post(context.Background(), path, nil, opts, nil)

    switch resp.StatusCode {
    case http.StatusNotFound:
        return scm.NamespaceNotFoundError(namespace)
    case http.StatusConflict:
        return scm.RepoExistError(name)
    default:
        return err
    }
}

func (cli *bitbucketClient) RemoveRepo(namespace, name string) error {
    path := fmt.Sprintf("/rest/api/1.0/projects/%s/repos/%s", namespace, name)
    resp, err := cli.Delete(context.Background(), path, nil, nil)

    switch resp.StatusCode {
    case http.StatusNotFound:
        return scm.RepoNotFoundError(name)
    default:
        return err
    }
}

func (cli *bitbucketClient) AddKey(namespace string, key string) error {
    opts := SSHKey{}
    opts.Key.Text = key
    opts.Permission = "PROJECT_WRITE"

    path := fmt.Sprintf("/rest/keys/1.0/projects/%s/ssh", namespace)
    resp, err := cli.Post(context.Background(), path, nil, opts, nil)

    if resp.StatusCode == http.StatusBadRequest {
        return scm.InvalidKeyError{}
    }
    return checkNamespaceError(namespace, resp.StatusCode, err)
}

func (cli *bitbucketClient) RemoveKey(namespace string, key string) error {
    ctx := context.Background()
    keys, err := cli.listKeys(ctx, namespace)
    if err != nil {
        return err
    }

    for _, k := range keys {
        if strings.TrimSpace(k.Key.Text) == strings.TrimSpace(key) {
            path := fmt.Sprintf("/rest/keys/1.0/projects/%s/ssh/%d", namespace, k.Key.Id)
            _, err := cli.Delete(ctx, path, nil, nil)
            if err != nil {
                return err
            }
        }
    }

    return nil
}

func (cli *bitbucketClient) ListKeys(namespace string) ([]string, error) {
    keys, err := cli.listKeys(context.Background(), namespace)
    if err != nil {
        return nil, err
    }

    result := make([]string, len(keys))
    for i, k := range keys {
        result[i] = k.Key.Text
    }
    return result, nil
}

func (cli *bitbucketClient) listKeys(ctx context.Context, namespace string) ([]SSHKey, error) {
    keys := make([]SSHKey, 0)
    start := 0

    for {
        path := fmt.Sprintf("/rest/keys/1.0/projects/%s/ssh", namespace)
        params := url.Values{"start": []string{strconv.Itoa(start)}}
        resp, err := cli.Get(ctx, path, params, nil)
        err = checkNamespaceError(namespace, resp.StatusCode, err)
        if err != nil {
            return keys, err
        }

        var page SSHKeyPage
        err = json.NewDecoder(resp.Body).Decode(&page)
        if err != nil {
            return keys, err
        }

        keys = append(keys, page.Values...)

        if page.IsLastPage {
            break
        }
        start = page.NextPageStart
    }

    return keys, nil
}

func checkNamespaceError(namespace string, status int, err error) error {
    switch status {
    case http.StatusNotFound:
        return scm.NamespaceNotFoundError(namespace)
    case http.StatusConflict:
        return scm.NamespaceExistError(namespace)
    default:
        return err
    }
}

var _ scm.SCM = &bitbucketClient{}

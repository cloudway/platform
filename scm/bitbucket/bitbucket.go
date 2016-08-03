package bitbucket

import (
    "io"
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
            "Authorization":     "Basic " + auth, // TODO
            "X-Atlassian-Token": "nocheck",
            "Accept":            "application/json",
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
    return checkNamespaceError(namespace, resp, err)
}

func (cli *bitbucketClient) RemoveNamespace(namespace string) error {
    ctx := context.Background()
    start := 0

    for {
        path := fmt.Sprintf("/rest/api/1.0/projects/%s/repos", namespace)
        params := url.Values{"start": []string{strconv.Itoa(start)}}
        resp, err := cli.Get(ctx, path, params, nil)
        err = checkNamespaceError(namespace, resp, err)
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
                return checkServerError(resp, err)
            }
        }

        if page.IsLastPage {
            break
        }
        start = page.NextPageStart
    }

    path := fmt.Sprintf("/rest/api/1.0/projects/%s", namespace)
    resp, err := cli.Delete(context.Background(), path, nil, nil)
    return checkNamespaceError(namespace, resp, err)
}

func (cli *bitbucketClient) CreateRepo(namespace, name string) error {
    opts := CreateRepoOpts{
        Name: name,
    }

    path := fmt.Sprintf("/rest/api/1.0/projects/%s/repos", namespace)
    resp, err := cli.Post(context.Background(), path, nil, opts, nil)

    if err != nil {
        switch resp.StatusCode {
        case http.StatusNotFound:
            return scm.NamespaceNotFoundError(namespace)
        case http.StatusConflict:
            return scm.RepoExistError(name)
        default:
            return checkServerError(resp, err)
        }
    }

    // enable post-receive hook
    const hookKey = "com.cloudway.bitbucket.plugins.repo-deployer:repo-deployer"
    path = fmt.Sprintf("/rest/api/1.0/projects/%s/repos/%s/settings/hooks/%s/enabled", namespace, name, hookKey)
    resp, err = cli.Put(context.Background(), path, nil, nil, nil)
    return checkServerError(resp, err)
}

func (cli *bitbucketClient) RemoveRepo(namespace, name string) error {
    path := fmt.Sprintf("/rest/api/1.0/projects/%s/repos/%s", namespace, name)
    resp, err := cli.Delete(context.Background(), path, nil, nil)

    switch resp.StatusCode {
    case http.StatusNotFound:
        return scm.RepoNotFoundError(name)
    default:
        return checkServerError(resp, err)
    }
}

func (cli *bitbucketClient) Populate(namespace, name string, payload io.Reader, size int64) error {
    path := fmt.Sprintf("/rest/deploy/1.0/projects/%s/repos/%s/populate", namespace, name)

    // check to see if repository already populated
    resp, err := cli.Head(context.Background(), path, nil, nil)
    if resp.StatusCode == http.StatusForbidden {
        return nil
    }
    if err != nil {
        return checkServerError(resp, err)
    }

    headers := map[string][]string {
        "Content-Type":   []string{"application/tar"},
        "Content-Length": []string{strconv.FormatInt(size, 10)},
    }
    resp, err = cli.PutRaw(context.Background(), path, nil, payload, headers)
    return checkNamespaceError(namespace, resp, err)
}

var allowedSchemes = []string{
    "git", "http", "https", "ftp", "ftps", "rsync",
}

func isAllowedScheme(scheme string) bool {
    for _, s := range allowedSchemes {
        if s == scheme {
            return true
        }
    }
    return false
}

func (cli *bitbucketClient) PopulateURL(namespace, name, remote string) error {
    u, err := url.Parse(remote)
    if err != nil {
        return err
    }
    if u.Scheme == "" || !isAllowedScheme(u.Scheme) {
        return fmt.Errorf("Unsupported Git clone scheme: %s", u.Scheme)
    }

    path := fmt.Sprintf("/rest/deploy/1.0/projects/%s/repos/%s/populate", namespace, name)

    // check to see if repository already populated
    resp, err := cli.Head(context.Background(), path, nil, nil)
    if resp.StatusCode == http.StatusForbidden {
        return nil
    }
    if err != nil {
        return checkServerError(resp, err)
    }

    // populate repository from template URL
    query := url.Values{"url": []string{remote}}
    resp, err = cli.Post(context.Background(), path, query, nil, nil)
    return checkNamespaceError(namespace, resp, err)
}

func (cli *bitbucketClient) Deploy(namespace, name string, branch string) error {
    path := fmt.Sprintf("/rest/deploy/1.0/projects/%s/repos/%s/deploy", namespace, name)
    query := url.Values{"branch": []string{branch}}
    resp, err := cli.Post(context.Background(), path, query, nil, nil)
    return checkNamespaceError(namespace, resp, err)
}

func (cli *bitbucketClient) GetDeploymentBranch(namespace, name string) (*scm.Branch, error) {
    path := fmt.Sprintf("/rest/deploy/1.0/projects/%s/repos/%s/settings", namespace, name)
    resp, err := cli.Get(context.Background(), path, nil, nil)
    if err = checkNamespaceError(namespace, resp, err); err != nil {
        return nil, err
    }

    var branch scm.Branch
    err = json.NewDecoder(resp.Body).Decode(&branch)
    return &branch, err
}

func (cli *bitbucketClient) GetDeploymentBranches(namespace, name string) ([]scm.Branch, error) {
    branches, err := cli.getRefs(namespace, name, "branches")
    if err != nil {
        return nil, err
    }

    tags, err := cli.getRefs(namespace, name, "tags")
    if err != nil {
        return nil, err
    }

    return append(branches, tags...), nil
}

func (cli *bitbucketClient) getRefs(namespace, name, typ string) ([]scm.Branch, error) {
    path := fmt.Sprintf("/rest/api/1.0/projects/%s/repos/%s/%s", namespace, name, typ)
    ctx := context.Background()

    refs := make([]scm.Branch, 0)
    start := 0

    for {
        params := url.Values{"start": []string{strconv.Itoa(start)}}
        resp, err := cli.Get(ctx, path, params, nil)
        err = checkNamespaceError(namespace, resp, err)
        if err != nil {
            return refs, err
        }

        var page BranchPage
        err = json.NewDecoder(resp.Body).Decode(&page)
        if err != nil {
            return refs, err
        }
        refs = append(refs, page.Values...)

        if page.IsLastPage {
            break
        }
        start = page.NextPageStart
    }

    return refs, nil
}

func (cli *bitbucketClient) AddKey(namespace string, key string) error {
    opts := SSHKey{}
    opts.Key.Text = key
    opts.Permission = "PROJECT_WRITE"

    path := fmt.Sprintf("/rest/keys/1.0/projects/%s/ssh", namespace)
    resp, err := cli.Post(context.Background(), path, nil, opts, nil)

    switch resp.StatusCode {
    case http.StatusBadRequest:
        return scm.InvalidKeyError{}
    case http.StatusConflict:
        return fmt.Errorf("SSH key already exists");
    default:
        return checkNamespaceError(namespace, resp, err)
    }
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
            resp, err := cli.Delete(ctx, path, nil, nil)
            if err != nil {
                return checkServerError(resp, err)
            }
        }
    }

    return nil
}

func (cli *bitbucketClient) ListKeys(namespace string) ([]scm.SSHKey, error) {
    keys, err := cli.listKeys(context.Background(), namespace)
    if err != nil {
        return nil, err
    }

    result := make([]scm.SSHKey, len(keys))
    for i, k := range keys {
        result[i].Label = k.Key.Label
        result[i].Text = k.Key.Text
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
        err = checkNamespaceError(namespace, resp, err)
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

func checkNamespaceError(namespace string, resp *rest.ServerResponse, err error) error {
    switch resp.StatusCode {
    case http.StatusNotFound:
        return scm.NamespaceNotFoundError(namespace)
    case http.StatusConflict:
        return scm.NamespaceExistError(namespace)
    default:
        return checkServerError(resp, err)
    }
}

func checkServerError(resp *rest.ServerResponse, err error) error {
    if se, ok := err.(rest.ServerError); ok {
        var errors ServerErrors
        if json.Unmarshal(se.RawError(), &errors) == nil {
            return errors
        }
    }
    return err
}

var _ scm.SCM = &bitbucketClient{}

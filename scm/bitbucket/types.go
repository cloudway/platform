package bitbucket

import (
    "bytes"
    "fmt"
    "github.com/cloudway/platform/scm"
)

type Page struct {
    Size            int  `json:"size"`
    Limit           int  `json:"limit"`
    IsLastPage      bool `json:"isLastPage"`
    NextPageStart   int  `json:"nextPageStart"`
}

type CreateProjectOpts struct {
    Key  string `json:"key"`
    Name string `json:"name"`
}

type CreateRepoOpts struct {
    Name string `json:"name"`
}

type Repo struct {
    Slug string `json:"slug"`
}

type RepoPage struct {
    Page
    Values []Repo `json:"values"`
}

type SSHKey struct {
    Key struct {
        Id    int       `json:"id"`
        Text  string    `json:"text"`
        Label string    `json:"label"`
    } `json:"key"`
    Permission string `json:"permission"`
}

type SSHKeyPage struct {
    Page
    Values []SSHKey `json:"values"`
}

type BranchPage struct {
    Page
    Values []scm.Branch `json:"values"`
}

type ServerErrors struct {
    Errors []struct {
        Context string `json:"context"`
        Message string `json:"message"`
    } `json:"errors"`
}

func (se ServerErrors) Error() string {
    if len(se.Errors) == 1 {
        return fmt.Sprintf("Error response from server: %s", se.Errors[0].Message)
    }

    var buf bytes.Buffer
    fmt.Fprintln(&buf, "Error response from server:")
    for _, e := range se.Errors {
        fmt.Fprintln(&buf, e.Message)
    }
    return buf.String()
}

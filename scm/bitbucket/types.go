package bitbucket

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
        Id   int      `json:"id,omitempty"`
        Text string   `json:"text"`
    } `json:"key"`
    Permission string `json:"permission"`
}

type SSHKeyPage struct {
    Page
    Values []SSHKey `json:"values"`
}

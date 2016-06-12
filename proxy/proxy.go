package proxy

import (
    "fmt"
    "errors"
    "net/url"
    "github.com/cloudway/platform/pkg/manifest"
)

type Proxy interface {
    AddEndpoints(id string, endpoints []*manifest.Endpoint) error
    RemoveEndpoints(id string) error
    Close() error
}

type proxyFunc func(*url.URL)(Proxy, error)
var proxyRegistry map[string]proxyFunc = make(map[string]proxyFunc)

func New(proxyUrl string) (Proxy, error) {
    if proxyUrl == "" {
        return nil, errors.New("Proxy URL not configured")
    }

    u, err := url.Parse(proxyUrl)
    if err != nil {
        return nil, err
    }

    fn := proxyRegistry[u.Scheme]
    if fn == nil {
        return nil, fmt.Errorf("Unsuported proxy scheme: %s", u.Scheme)
    } else {
        return fn(u)
    }
}

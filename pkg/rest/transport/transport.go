package transport

import (
    "fmt"
    "time"
    "os"
    "strings"
    "net"
    "net/url"
    "net/http"
    "golang.org/x/net/proxy"
)

// apiTransport holds information about the http transport to connect with the API.
type apiTransport struct {
    *http.Client
    *tlsInfo
    transport *http.Transport
}

// NewTransportWithHTTP creates a new transport based on the provided proto,
// address and http client. It uses default http transport configuration if
// the client is nil. It does not modify the client's transport if it's not nil.
func NewTransportWithHTTP(proto, addr string, client *http.Client) (Client, error) {
    var transport *http.Transport

    if client != nil {
        tr, ok := client.Transport.(*http.Transport)
        if !ok {
            return nil, fmt.Errorf("unable to verify TLS configuration, invalid transport %v", client.Transport)
        }
        transport = tr
    } else {
        transport = defaultTransport(proto, addr)
        client = &http.Client{
            Transport: transport,
        }
    }

    return &apiTransport{
        Client:     client,
        tlsInfo:    &tlsInfo{transport.TLSClientConfig},
        transport:  transport,
    }, nil
}

// CancelRequest stops a request execution.
func (a *apiTransport) CancelRequest(req *http.Request) {
    a.transport.CancelRequest(req)
}

// defaultTransport creates a new http.Transport with default configuration.
func defaultTransport(proto, addr string) *http.Transport {
    tr := new(http.Transport)
    configureTransport(tr, proto, addr)
    return tr
}

var _ Client = &apiTransport{}

const defaultTimeout = 32 * time.Second

// configureTransport configures the specified Transport according to the
// specified proto and addr. If the proto is unix (using a unix socket to
// communicate) the compression is disabled.
func configureTransport(tr *http.Transport, proto, addr string) error {
    switch (proto) {
    case "unix":
        tr.DisableCompression = true
        tr.Dial = func(_, _ string) (net.Conn, error) {
            return net.DialTimeout(proto, addr, defaultTimeout)
        }
    default:
        tr.Proxy = http.ProxyFromEnvironment
        dialer, err := dialerFromEnvironment(&net.Dialer{
            Timeout: defaultTimeout,
        })
        if err != nil {
            return err
        }
        tr.Dial = dialer.Dial
    }
    return nil
}

// dialerFromEnvironment takes in a "direct" *net.Dailer and returns a
// proxy.Dailer which will route the connections through the proxy using
// the given dailer.
func dialerFromEnvironment(direct *net.Dialer) (proxy.Dialer, error) {
    allProxy := getProxyEnv("all_proxy")
    if len(allProxy) == 0 {
        return direct, nil
    }

    proxyURL, err := url.Parse(allProxy)
    if err != nil {
        return direct, err
    }

    proxyFromURL, err := proxy.FromURL(proxyURL, direct)
    if err != nil {
        return direct, err
    }

    noProxy := getProxyEnv("no_proxy")
    if len(noProxy) == 0 {
        return proxyFromURL, nil
    }

    perHost := proxy.NewPerHost(proxyFromURL, direct)
    perHost.AddFromString(noProxy)

    return perHost, nil
}

// getProxyEnv allows access to the uppercase and the lowercase forms of
// proxy-related variables.
func getProxyEnv(key string) string {
    proxyValue := os.Getenv(strings.ToUpper(key))
    if proxyValue == "" {
        return os.Getenv(strings.ToLower(key))
    }
    return proxyValue
}

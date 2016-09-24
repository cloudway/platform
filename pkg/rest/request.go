package rest

import (
	"bytes"
	"context"
	"encoding/json"
	"fmt"
	"io"
	"io/ioutil"
	"net/http"
	"net/url"
	"strings"

	"github.com/cloudway/platform/pkg/rest/transport/cancellable"
)

// serverResponse is a wrapper for http API responses.
type ServerResponse struct {
	Body       io.ReadCloser
	Header     http.Header
	StatusCode int
}

func (response *ServerResponse) EnsureClosed() {
	if response.Body != nil {
		// Drain up to 512 bytes and close the body to let the Transport reuse the connection
		io.CopyN(ioutil.Discard, response.Body, 512)
		response.Body.Close()
	}
}

// Head sends an http request to the API server using the method HEAD.
func (cli *Client) Head(ctx context.Context, path string, query url.Values, headers map[string][]string) (*ServerResponse, error) {
	return cli.sendRequest(ctx, "HEAD", path, query, nil, headers)
}

// Get sends an http request to the API server using the method GET.
func (cli *Client) Get(ctx context.Context, path string, query url.Values, headers map[string][]string) (*ServerResponse, error) {
	return cli.sendRequest(ctx, "GET", path, query, nil, headers)
}

// Post sends an http request to the API server using the method POST
func (cli *Client) Post(ctx context.Context, path string, query url.Values, obj interface{}, headers map[string][]string) (*ServerResponse, error) {
	return cli.sendRequest(ctx, "POST", path, query, obj, headers)
}

func (cli *Client) PostRaw(ctx context.Context, path string, query url.Values, body io.Reader, headers map[string][]string) (*ServerResponse, error) {
	return cli.sendClientRequest(ctx, "POST", path, query, body, headers)
}

// Put sends an http request to the API server using the method PUT
func (cli *Client) Put(ctx context.Context, path string, query url.Values, obj interface{}, headers map[string][]string) (*ServerResponse, error) {
	return cli.sendRequest(ctx, "PUT", path, query, obj, headers)
}

func (cli *Client) PutRaw(ctx context.Context, path string, query url.Values, body io.Reader, headers map[string][]string) (*ServerResponse, error) {
	return cli.sendClientRequest(ctx, "PUT", path, query, body, headers)
}

// Delete sends an http request to the API server using the method DELETE
func (cli *Client) Delete(ctx context.Context, path string, query url.Values, headers map[string][]string) (*ServerResponse, error) {
	return cli.sendRequest(ctx, "DELETE", path, query, nil, headers)
}

func (cli *Client) sendRequest(ctx context.Context, method, path string, query url.Values, obj interface{}, headers map[string][]string) (*ServerResponse, error) {
	var body io.Reader

	if obj != nil {
		var err error
		body, err = encodeData(obj)
		if err != nil {
			return nil, err
		}
		if headers == nil {
			headers = make(map[string][]string)
		}
		headers["Content-Type"] = []string{"application/json"}
	}

	return cli.sendClientRequest(ctx, method, path, query, body, headers)
}

func (cli *Client) sendClientRequest(ctx context.Context, method, path string, query url.Values, body io.Reader, headers map[string][]string) (*ServerResponse, error) {
	serverResp := &ServerResponse{
		Body:       nil,
		StatusCode: -1,
	}

	expectedPayload := (method == "POST" || method == "PUT")
	if expectedPayload && body == nil {
		body = bytes.NewReader([]byte{})
	}

	req, err := cli.newRequest(method, path, query, body, headers)
	if err != nil {
		return serverResp, err
	}

	if cli.proto == "unix" {
		// For local communications, it doesn't matter what the host is. We just
		// need a valid and meaningful host name.
		req.Host = "localhost"
	}
	req.URL.Host = cli.addr
	req.URL.Scheme = cli.transport.Scheme()

	resp, err := cancellable.Do(ctx, cli.transport, req)
	if err != nil {
		if isTimeout(err) || strings.Contains(err.Error(), "connection refused") ||
			strings.Contains(err.Error(), "dial unix") {
			return serverResp, ErrConnectionFailed
		}

		if !cli.transport.Secure() && strings.Contains(err.Error(), "malformed HTTP response") {
			return serverResp, fmt.Errorf("%v.\n* Are you trying to connect to a TLS-enabled daemon without TLS?", err)
		}

		if cli.transport.Secure() && strings.Contains(err.Error(), "remote error: bad certificate") {
			return serverResp, fmt.Errorf("The server probably has client authentication. "+
				"Please check your TLS client certification settings: %v",
				err)
		}

		return serverResp, fmt.Errorf("An error occurred trying to connect: %v", err)
	}

	if resp != nil {
		serverResp.StatusCode = resp.StatusCode
	}

	if serverResp.StatusCode < 200 || serverResp.StatusCode >= 400 {
		body, err := ioutil.ReadAll(resp.Body)
		if err != nil {
			return serverResp, err
		}
		if len(body) == 0 {
			message := fmt.Sprintf("Error: request returned %s for API route and version %s, "+
				"check if the server supports the requested API version",
				http.StatusText(serverResp.StatusCode), req.URL)
			return serverResp, ServerError{serverResp.StatusCode, []byte(message)}
		}
		return serverResp, ServerError{serverResp.StatusCode, bytes.TrimSpace(body)}
	}

	serverResp.Body = resp.Body
	serverResp.Header = resp.Header
	return serverResp, nil
}

func (cli *Client) newRequest(method, path string, query url.Values, body io.Reader, headers map[string][]string) (*http.Request, error) {
	apiPath := cli.getAPIPath(path, query)
	req, err := http.NewRequest(method, apiPath, body)
	if err != nil {
		return nil, err
	}

	for k, v := range cli.customHTTPHeaders {
		req.Header.Set(k, v)
	}

	if headers != nil {
		for k, v := range headers {
			req.Header[k] = v
		}
	}

	return req, nil
}

func encodeData(data interface{}) (*bytes.Buffer, error) {
	params := bytes.NewBuffer(nil)
	if data != nil {
		if err := json.NewEncoder(params).Encode(data); err != nil {
			return nil, err
		}
	}
	return params, nil
}

func isTimeout(err error) bool {
	type timeout interface {
		Timeout() bool
	}
	e := err
	switch urlErr := err.(type) {
	case *url.Error:
		e = urlErr.Err
	}
	t, ok := e.(timeout)
	return ok && t.Timeout()
}

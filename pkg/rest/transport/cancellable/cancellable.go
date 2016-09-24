// Package cancellable provides helper function to cancel http requests.
package cancellable

import (
	"context"
	"io"
	"net/http"

	"github.com/cloudway/platform/pkg/rest/transport"
)

// Do sends an HTTP request with the provided transport.Sender and returns an
// HTTP response. If the client is nil, http.DefaultClient is used. If the
// context is canceled or times out, ctx.Err() will be returned.
func Do(ctx context.Context, client transport.Sender, req *http.Request) (*http.Response, error) {
	if client == nil {
		client = http.DefaultClient
	}

	cancel := canceler(client, req)

	type responseAndError struct {
		resp *http.Response
		err  error
	}
	result := make(chan responseAndError, 1)

	go func() {
		resp, err := client.Do(req)
		result <- responseAndError{resp, err}
	}()

	var resp *http.Response

	select {
	case <-ctx.Done():
		cancel()
		// Clean up after the goroutine calling client.Do
		go func() {
			if r := <-result; r.resp != nil && r.resp.Body != nil {
				r.resp.Body.Close()
			}
		}()
		return nil, ctx.Err()
	case r := <-result:
		var err error
		resp, err = r.resp, r.err
		if err != nil {
			return resp, err
		}
	}

	c := make(chan struct{})
	go func() {
		select {
		case <-ctx.Done():
			cancel()
		case <-c:
			// The response's Body is closed.
		}
	}()
	resp.Body = &notifyingReader{resp.Body, c}

	return resp, nil
}

func canceler(client transport.Sender, req *http.Request) func() {
	ch := make(chan struct{})
	req.Cancel = ch

	return func() {
		close(ch)
	}
}

// notifyingReader is an io.ReadCloser that closes the notify channel after
// Close is called or a Read fails on the underlying ReadCloser.
type notifyingReader struct {
	io.ReadCloser
	notify chan<- struct{}
}

func (r *notifyingReader) Read(p []byte) (int, error) {
	n, err := r.ReadCloser.Read(p)
	if err != nil && r.notify != nil {
		close(r.notify)
		r.notify = nil
	}
	return n, err
}

func (r *notifyingReader) Close() error {
	err := r.ReadCloser.Close()
	if r.notify != nil {
		close(r.notify)
		r.notify = nil
	}
	return err
}

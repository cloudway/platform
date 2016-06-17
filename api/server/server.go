package server

import (
    "net"
    "net/http"
    "strings"

    "golang.org/x/net/context"
    "github.com/gorilla/mux"
    "github.com/Sirupsen/logrus"

    "github.com/cloudway/platform/api/server/router"
    "github.com/cloudway/platform/api/server/middleware"
    "github.com/cloudway/platform/api/server/httputils"
)

// versionMatcher defines a variable matcher to be parsed by the router
// when a request is about to be served.
const versionMatcher = "/v{version:[0-9.]+}"

// Server contains instance details for the server
type Server struct {
    servers     []*HTTPServer
    middlewares []middleware.Middleware
    routers     []router.Router
    contextRoot string
    Mux         *mux.Router
}

// New returns a new instance of the server based on the specified configuration.
// It allocates resources which will be needed for ServeAPI(ports, unix-sockets).
func New(contextRoot string) *Server {
    return &Server{contextRoot: contextRoot}
}

// UseMiddleware appends a new middleware to the request chain.
// This needs to be called before the API routes are configured.
func (s *Server) UseMiddleware(m middleware.Middleware) {
    s.middlewares = append(s.middlewares, m)
}

// Accept sets a listener the server accepts connections into.
func (s *Server) Accept(addr string, listener net.Listener) {
    httpServer := &HTTPServer{
        srv: &http.Server{
            Addr: addr,
        },
        l: listener,
    }
    s.servers = append(s.servers, httpServer)
}

// Close closes servers and thus stop receiving requests
func (s *Server) Close() {
    for _, srv := range s.servers {
        if err := srv.Close(); err != nil {
            logrus.Error(err)
        }
    }
}

// serveAPI loops through all initialized servers and spawns goroutine
// with Server method for each. It sets createMux() as Handler also.
func (s *Server) serveAPI() error {
    var chErrors = make(chan error, len(s.servers))
    for _, srv := range s.servers {
        srv.srv.Handler = s.Mux
        go func(srv *HTTPServer) {
            var err error
            logrus.Infof("API server listen on %s", srv.l.Addr())
            if err = srv.Serve(); err != nil && strings.Contains(err.Error(), "use of closed network connection") {
                err = nil
            }
            chErrors <- err
        }(srv)
    }

    for i := 0; i < len(s.servers); i++ {
        err := <-chErrors
        if err != nil {
            return err
        }
    }

    return nil
}

// HTTPServer contains an instance of http server and the listener.
type HTTPServer struct {
    srv *http.Server
    l   net.Listener
}

// Serve starts listening for inbound requests.
func (s *HTTPServer) Serve() error {
    return s.srv.Serve(s.l)
}

// Close closes the HTTPServer from listening for the inbound requests.
func (s *HTTPServer) Close() error {
    return s.l.Close()
}

func (s *Server) makeHTTPHandler(handler httputils.APIFunc) http.HandlerFunc {
    return func(w http.ResponseWriter, r *http.Request) {
        // Define the context that we'll pass around to share info
        //
        // The 'context' will be used for global data that should
        // apply to all requests. Data that is specific to the
        // immediate function being called should still be passed
        // as 'args' on the function call.
        ctx := context.Background()
        handlerFunc := s.handleWithGlobalMiddlewares(handler)

        vars := mux.Vars(r)
        if vars == nil {
            vars = make(map[string]string)
        }

        if err := handlerFunc(ctx, w, r, vars); err != nil {
            httputils.WriteError(w, r, err)
        }
    }
}

// InitRouter initializes the list of routers for the server.
func (s *Server) InitRouter(routers ...router.Router) {
    for _, r := range routers {
        s.routers = append(s.routers, r)
    }
    s.Mux = s.createMux()
}

// createMux initializes the main router the server uses.
func (s *Server) createMux() *mux.Router {
    m := mux.NewRouter()

    logrus.Debugf("Regitering routers")
    for _, apiRouter := range s.routers {
        for _, r := range apiRouter.Routes() {
            f := s.makeHTTPHandler(r.Handler())

            logrus.Debugf("Registering %s %s", r.Method(), r.Path())
            m.Path(s.contextRoot + versionMatcher + r.Path()).Methods(r.Method()).Handler(f)
            m.Path(s.contextRoot + r.Path()).Methods(r.Method()).Handler(f)
        }
    }

    return m
}

// Wait blocks the server goroutine until it exists.
// It sends an error message if there is any error during
// the API execution.
func (s *Server) Wait(waitChan chan error) {
    if err := s.serveAPI(); err != nil {
        logrus.WithError(err).Error("ServeAPI error")
        waitChan <- err
        return
    }
    waitChan <- nil
}

// handleWithGlobalMiddlewares wraps the handler function for a request with
// the server's global middlewares. The order of the middles is backwards,
// meaning that the first in the list will be evaluated last.
func (s *Server) handleWithGlobalMiddlewares(handler httputils.APIFunc) httputils.APIFunc {
    next := handler
    for _, m := range s.middlewares {
        next = m.WrapHandler(next)
    }
    return next
}

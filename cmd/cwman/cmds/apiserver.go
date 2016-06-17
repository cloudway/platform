package cmds

import (
    "os"
    "os/signal"
    "net"
    "runtime"
    "syscall"
    "sync/atomic"

    "golang.org/x/net/context"
    "github.com/Sirupsen/logrus"
    "github.com/docker/engine-api/types"
    "github.com/cloudway/platform/api"
    "github.com/cloudway/platform/api/server"
    "github.com/cloudway/platform/api/server/router/system"
    "github.com/cloudway/platform/api/server/middleware"
    "github.com/cloudway/platform/api/server/auth"
    "github.com/cloudway/platform/container"
)

func (cli *CWMan) CmdAPIServer(args ...string) error {
    var addr string

    cmd := cli.Subcmd("api-server")
    cmd.StringVar(&addr, []string{"-bind"}, ":6616", "API server bind address")
    cmd.ParseFlags(args, true)

    stopc := make(chan bool)
    defer close(stopc)

    docker := cli.DockerClient
    version, err := docker.ServerVersion(context.Background())
    if err != nil {
        return err
    }

    authz, err := auth.NewAuthenticator()
    if err != nil {
        return err
    }

    api := server.New()

    l, err := net.Listen("tcp", addr)
    if err != nil {
        return err
    }
    api.Accept(addr, l)

    initMiddlewares(api, version, authz)
    initRouters(api, docker, authz)

    // The serve API routine never exists unless an error occurs
    // we need to start it as a goroutine and wait on it so
    // daemon doesn't exit
    waitChan := make(chan error)
    go api.Wait(waitChan)
    trapSignals(func() {
        api.Close()
        <-stopc // wait for CmdServer() to return
    })

    // Server is fully initialized and handling API traffic.
    // Wait for serve API to complete
    apiErr := <-waitChan
    if apiErr != nil {
        logrus.WithError(apiErr).Error("API server error")
    }
    logrus.Info("API server terminated")
    return nil
}

func initMiddlewares(s *server.Server, v types.Version, authz *auth.Authenticator) {
    vm := middleware.NewVersionMiddleware(api.Version, api.MinVersion, v.Version)
    s.UseMiddleware(vm)

    s.UseMiddleware(middleware.NewAuthMiddleware(authz))
}

func initRouters(s *server.Server, cli container.DockerClient, authz *auth.Authenticator) {
    s.InitRouter(
        system.NewRouter(cli, authz),
    )
}

func trapSignals(cleanup func()) {
    c := make(chan os.Signal, 1)
    signal.Notify(c, os.Interrupt, syscall.SIGTERM, syscall.SIGQUIT)
    go func() {
        var interruptCount uint32
        for sig := range c {
            go func(sig os.Signal) {
                logrus.Infof("*%s*", sig)
                switch sig {
                case os.Interrupt, syscall.SIGTERM:
                    if atomic.LoadUint32(&interruptCount) < 3 {
                        // Initiate the cleanup only once
                        if atomic.AddUint32(&interruptCount, 1) == 1 {
                            // Call the provided cleanup handler
                            cleanup()
                            os.Exit(0)
                        } else {
                            return
                        }
                    } else {
                        // 3 SIGTERM/INT signals received; force exit without cleanup
                        logrus.Warnf("Forcing shutdown without cleanup")
                        os.Exit(128)
                    }
                case syscall.SIGQUIT:
                    dumpStacks()
                }
            }(sig)
        }
    }()
}

func dumpStacks() {
    var buf []byte
    var stackSize int
    bufferLen := 16384
    for stackSize == len(buf) {
        buf = make([]byte, bufferLen)
        stackSize = runtime.Stack(buf, true)
        bufferLen *= 2
    }
    buf = buf[:stackSize]
    logrus.Infof("=== BEGIN goroutine stack dump ===\n%s\n=== END goroutine stack dump ===", buf)
}

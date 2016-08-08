package cmds

import (
    "os"
    "os/signal"
    "net"
    prof "runtime"
    "syscall"
    "sync/atomic"
    "github.com/Sirupsen/logrus"

    "github.com/cloudway/platform/api/server"
    "github.com/cloudway/platform/api/server/middleware"
    "github.com/cloudway/platform/api/server/router/system"
    "github.com/cloudway/platform/api/server/router/plugins"
    "github.com/cloudway/platform/api/server/router/applications"
    "github.com/cloudway/platform/broker"
    "github.com/cloudway/platform/console"
)

const _CONTEXT_ROOT = "/api"

func (cli *CWMan) CmdAPIServer(args ...string) (err error) {
    var addr string

    cmd := cli.Subcmd("api-server")
    cmd.StringVar(&addr, []string{"-bind"}, ":6616", "API server bind address")
    cmd.ParseFlags(args, true)

    stopc := make(chan bool)
    defer close(stopc)

    br, err := broker.New(cli.DockerClient)
    if err != nil {
        return err
    }

    con, err := console.NewConsole(br)
    if err != nil {
        return err
    }

    api := server.New(_CONTEXT_ROOT)

    l, err := net.Listen("tcp", addr)
    if err != nil {
        return err
    }
    api.Accept(addr, l)

    initMiddlewares(api, br)
    initRouters(api, br)

    con.InitRoutes(api)

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

func initMiddlewares(s *server.Server, br *broker.Broker) {
    s.UseMiddleware(middleware.NewVersionMiddleware(br))
    s.UseMiddleware(middleware.NewAuthMiddleware(br, _CONTEXT_ROOT))
}

func initRouters(s *server.Server, br *broker.Broker) {
    s.InitRouter(
        system.NewRouter(br),
        plugins.NewRouter(br),
        applications.NewRouter(br),
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
        stackSize = prof.Stack(buf, true)
        bufferLen *= 2
    }
    buf = buf[:stackSize]
    logrus.Infof("=== BEGIN goroutine stack dump ===\n%s\n=== END goroutine stack dump ===", buf)
}

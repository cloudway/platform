package sandbox

import (
    "os"
    "os/user"
    "io/ioutil"
    "net"
    "errors"
    "strings"
    "strconv"
    "path/filepath"
    "github.com/Sirupsen/logrus"
    "github.com/cloudway/platform/pkg/manifest"
)

type Application struct {
    name        string
    namespace   string
    home        string
    user        string
    uid         int
    gid         int
}

func NewApplication() *Application {
    var (
        name      = os.Getenv("CLOUDWAY_APP_NAME")
        namespace = os.Getenv("CLOUDWAY_APP_NAMESPACE")
        home      = os.Getenv("CLOUDWAY_HOME_DIR")
        username  = os.Getenv("CLOUDWAY_APP_USER")
    )

    if home == "" || name == "" || namespace == "" {
        logrus.Fatal("Invalid application environment")
    }

    uid, gid := 0, 0
    if username != "" {
        if u, err := user.Lookup(username); err != nil {
            logrus.Error(err)
        } else {
            uid, _ = strconv.Atoi(u.Uid)
            gid, _ = strconv.Atoi(u.Gid)
        }
    }

    return &Application{
        name:       name,
        namespace:  namespace,
        home:       home,
        user:       username,
        uid:        uid,
        gid:        gid,
    }
}

func (app *Application) HomeDir() string {
    return app.home
}

func (app *Application) EnvDir() string {
    return filepath.Join(app.home, ".env")
}

func (app *Application) RepoDir() string {
    return filepath.Join(app.HomeDir(), "repo")
}

func (app *Application) DataDir() string {
    return filepath.Join(app.HomeDir(), "data")
}

func (app *Application) LogDir() string {
    return filepath.Join(app.HomeDir(), "logs")
}

func (app *Application) Name() string {
    return app.name
}

func (app *Application) Namespace() string {
    return app.namespace
}

func (app *Application) FQDN() string {
    return os.Getenv("CLOUDWAY_APP_DNS")
}

func (app *Application) GetExtraHosts() []string {
    extraHosts := app.Getenv("CLOUDWAY_EXTRA_HOSTS")
    if extraHosts == "" {
        return nil
    } else {
        return strings.Split(extraHosts, ",")
    }
}

func (app *Application) GetLocalIP() (string, error) {
    addrs, err := net.InterfaceAddrs()
    if err != nil {
        return "", err
    }

    for _, address := range addrs {
        // check the address type and if it is not a loopback
        if ipnet, ok := address.(*net.IPNet); ok && !ipnet.IP.IsLoopback() {
            if ipnet.IP.To4() != nil {
                return ipnet.IP.String(), nil
            }
        }
    }

    return "", errors.New("No local IP address found")
}

func (app *Application) GetPlugins() (map[string]*manifest.Plugin, error) {
    files, err := ioutil.ReadDir(app.HomeDir())
    if err != nil {
        return nil, err
    }

    plugins := make(map[string]*manifest.Plugin)
    for _, file := range files {
        subdir := filepath.Join(app.HomeDir(), file.Name())
        if manifest.IsPluginDir(subdir) {
            if p, err := manifest.Load(subdir); err != nil {
                logrus.WithError(err).Errorf("Faield to load plugin %s", file.Name())
            } else {
                plugins[p.Name] = p
            }
        }
    }
    return plugins, nil
}

func (app *Application) GetEndpoints(ip string) ([]*manifest.Endpoint, error) {
    plugins, err := app.GetPlugins()
    if err != nil {
        return nil, err
    }

    if ip == "" {
        ip, err = app.GetLocalIP()
        if err != nil {
            return nil, err
        }
    }

    fqdn := app.FQDN()
    extra := app.GetExtraHosts()

    var endpoints []*manifest.Endpoint
    for _, p := range plugins {
        endpoints = append(endpoints, p.GetEndpoints(fqdn, ip)...)
        for _, host := range extra {
            endpoints = append(endpoints, p.GetEndpoints(host, ip)...)
        }
    }
    return endpoints, nil
}

func (app *Application) CreatePrivateEndpoints(ip string) error {
    plugins, err := app.GetPlugins()
    if err != nil {
        return err
    }

    if ip == "" {
        ip, err = app.GetLocalIP()
        if err != nil {
            return err
        }
    }

    for _, p := range plugins {
        for _, ep := range p.GetEndpoints(app.FQDN(), ip) {
            app.SetPluginEnv(p, ep.PrivateHostName, ip, true)
            app.SetPluginEnv(p, ep.PrivatePortName, strconv.Itoa(int(ep.PrivatePort)), true)
        }
    }

    return nil
}

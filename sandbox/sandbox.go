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

type Sandbox struct {
    name        string
    namespace   string
    home        string
    user        string
    uid         int
    gid         int
}

func New() *Sandbox {
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

    return &Sandbox{
        name:       name,
        namespace:  namespace,
        home:       home,
        user:       username,
        uid:        uid,
        gid:        gid,
    }
}

func (box *Sandbox) HomeDir() string {
    return box.home
}

func (box *Sandbox) EnvDir() string {
    return filepath.Join(box.home, ".env")
}

func (box *Sandbox) RepoDir() string {
    return filepath.Join(box.HomeDir(), "repo")
}

func (box *Sandbox) DeployDir() string {
    return filepath.Join(box.HomeDir(), "deploy")
}

func (box *Sandbox) DataDir() string {
    return filepath.Join(box.HomeDir(), "data")
}

func (box *Sandbox) LogDir() string {
    return filepath.Join(box.HomeDir(), "logs")
}

func (box *Sandbox) Name() string {
    return box.name
}

func (box *Sandbox) Namespace() string {
    return box.namespace
}

func (box *Sandbox) ServiceName() string {
    return os.Getenv("CLOUDWAY_SERVICE_NAME")
}

func (box *Sandbox) FQDN() string {
    return os.Getenv("CLOUDWAY_APP_DNS")
}

func (box *Sandbox) ExtraHosts() []string {
    extraHosts := box.Getenv("CLOUDWAY_EXTRA_HOSTS")
    if extraHosts == "" {
        return nil
    } else {
        return strings.Split(extraHosts, ",")
    }
}

func (box *Sandbox) LocalIP() (string, error) {
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

func (box *Sandbox) Plugins() (map[string]*manifest.Plugin, error) {
    files, err := ioutil.ReadDir(box.HomeDir())
    if err != nil {
        return nil, err
    }

    plugins := make(map[string]*manifest.Plugin)
    for _, file := range files {
        subdir := filepath.Join(box.HomeDir(), file.Name())
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

func (box *Sandbox) PrimaryPlugin() (*manifest.Plugin, error) {
    plugins, err := box.Plugins()
    if err != nil {
        return nil, err
    }
    for _, p := range plugins {
        if p.IsFramework() {
            return p, nil
        }
    }
    return nil, errors.New("Primary plugin not found in this container")
}

func (box *Sandbox) GetEndpoints(ip string) ([]*manifest.Endpoint, error) {
    plugins, err := box.Plugins()
    if err != nil {
        return nil, err
    }

    if ip == "" {
        ip, err = box.LocalIP()
        if err != nil {
            return nil, err
        }
    }

    fqdn := box.FQDN()
    service := box.ServiceName()
    extra := box.ExtraHosts()

    var endpoints []*manifest.Endpoint
    for _, p := range plugins {
        endpoints = append(endpoints, p.GetEndpoints(fqdn, service, ip)...)
        for _, host := range extra {
            endpoints = append(endpoints, p.GetEndpoints(host, service, ip)...)
        }
    }
    return endpoints, nil
}

func (box *Sandbox) CreatePrivateEndpoints(ip string) error {
    plugins, err := box.Plugins()
    if err != nil {
        return err
    }

    if ip == "" {
        ip, err = box.LocalIP()
        if err != nil {
            return err
        }
    }

    for _, p := range plugins {
        for _, ep := range p.GetEndpoints(box.FQDN(), box.ServiceName(), ip) {
            box.SetPluginEnv(p, ep.PrivateHostName, ip, true)
            box.SetPluginEnv(p, ep.PrivatePortName, strconv.Itoa(int(ep.PrivatePort)), true)
        }
    }

    return nil
}

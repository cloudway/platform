package plugin

// This package is exported to sandbox and container manager, please
// keep it clean.

import (
    "fmt"
    "strings"
    "os"
    "io"
    "io/ioutil"
    "path/filepath"
    "gopkg.in/yaml.v2"
)

type Plugin struct {
    Path            string `yaml:"-"`
    Name            string `yaml:"Name"`
    DisplayName     string `yaml:"Display-Name"`
    Description     string `yaml:"Description"`
    Version         string `yaml:"Version"`
    Vendor          string `yaml:"Vendor"`
    PluginVersion   string `yaml:"Plugin-Version"`
    PluginVendor    string `yaml:"Plugin-Vendor"`
    Category        string `yaml:"Category"`
    BaseImage       string `yaml:"Base-Image"`
    Endpoints       []*Endpoint `yaml:"Endpoints,omitempty"`
}

type Endpoint struct {
    PrivateHostName string `yaml:"Private-Host-Name"`
    PrivatePortName string `yaml:"Private-Port-Name"`
    PrivateHost     string `yaml:"-"`
    PrivatePort     int32  `yaml:"Private-Port"`
    ProxyMappings   []*ProxyMapping `yaml:"Proxy-Mappings,omitempty"`
}

type ProxyMapping struct {
    Frontend        string `yaml:"Frontend"`
    Backend         string `yaml:"Backend"`
    Protocols       []string `yaml:"Protocols,omitempty"`
    Protocol        string `yaml:"-"`
}

const ManifestEntry = "metadata/plugin.yml"

func manifestFile(dir string) string {
    return filepath.Join(dir, filepath.FromSlash(ManifestEntry))
}

func IsPluginDir(dir string) bool {
    _, err := os.Stat(manifestFile(dir))
    return err == nil
}

func Load(path string, env map[string]string) (*Plugin, error) {
    f, err := os.Open(manifestFile(path))
    if err != nil {
        return nil, err
    }
    defer f.Close()

    plugin, err := ReadManifest(f)
    if err != nil {
        return nil, err
    }

    plugin.Path = path
    if env != nil {
        initEndpoints(plugin, env)
    }
    return plugin, nil
}

func ReadManifest(f io.Reader) (*Plugin, error) {
    data, err := ioutil.ReadAll(f)
    if err != nil {
        return nil, err
    }

    plugin := &Plugin{}
    err = yaml.Unmarshal(data, plugin)
    return plugin, err
}

func initEndpoints(p *Plugin, env map[string]string) {
    for _, ep := range p.Endpoints {
        envPrefix := "CLOUDWAY_" + p.Name + "_"
        ep.PrivateHostName = strings.ToUpper(envPrefix + ep.PrivateHostName)
        ep.PrivatePortName = strings.ToUpper(envPrefix + ep.PrivatePortName)
        ep.PrivateHost     = env[ep.PrivateHostName]

        for _, pm := range ep.ProxyMappings {
            if len(pm.Protocols) == 0 {
                pm.Protocols = []string{"http"}
            }
        }
    }
}

func (p *Plugin) CopyOf(path string) *Plugin {
    var copy Plugin = *p
    copy.Path = path
    return &copy
}

func (p *Plugin) GetProxyMappings() ([]ProxyMapping) {
    var res = make([]ProxyMapping, 0, 4)
    for _, ep := range p.Endpoints {
        for _, m := range ep.ProxyMappings {
            for _, prot := range m.Protocols {
                m2 := ProxyMapping{
                    Frontend:  getFrontendUri(m.Frontend),
                    Backend:   getBackendUri(ep, m.Backend, prot),
                    Protocol:  prot,
                }
                res = append(res, m2)
            }
        }
    }
    return res
}

func getFrontendUri(uri string) string {
    if strings.HasSuffix(uri, "/") {
        uri = uri[0 : len(uri)-1]
    }
    if len(uri) != 0 && !strings.HasPrefix(uri, "/") {
        uri = "/" + uri
    }
    return uri
}

func getBackendUri(ep *Endpoint, uri, protocol string) string {
    if strings.HasPrefix(uri, "/") && strings.HasSuffix(uri, "/") {
        uri = uri[0 : len(uri)-1]
    }
    if len(uri) == 0 || strings.HasPrefix(uri, "/") {
        return fmt.Sprintf("%s://%s:%d%s", protocol, ep.PrivateHost, ep.PrivatePort, uri)
    } else {
        return uri // GONE, FORBIDDEN, REDIRECT:/url, etc
    }
}

func (p *Plugin) IsFramework() bool {
    return p.Category == "Framework"
}
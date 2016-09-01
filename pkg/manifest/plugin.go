package manifest

// This package is exported to sandbox and container manager, please
// keep it clean.

import (
	"fmt"
	"io"
	"io/ioutil"
	"os"
	"path/filepath"
	"strings"

	"gopkg.in/yaml.v2"
)

type Category string

const (
	Framework Category = "Framework"
	Service   Category = "Service"
	Library   Category = "Library"
)

func (cat Category) IsFramework() bool {
	return cat == Framework
}

func (cat Category) IsService() bool {
	return cat == Service
}

func (cat Category) IsLibrary() bool {
	return cat == Library
}

type Plugin struct {
	Path        string      `yaml:"-" json:",omitempty"`
	Tag         string      `yaml:"-" json:",omitempty"`
	Name        string      `yaml:"Name"`
	DisplayName string      `yaml:"Display-Name"`
	Description string      `yaml:"Description,omitempty"`
	Version     string      `yaml:"Version"`
	Vendor      string      `yaml:"Vendor"`
	Shared      bool        `yaml:"Shared,omitempty" json:",omitempty"`
	Logo        string      `yaml:"Logo,omitempty" json:",omitempty"`
	Category    Category    `yaml:"Category"`
	BaseImage   string      `yaml:"Base-Image"`
	BuildCache  []string    `yaml:"Build-Cache" json:",omitempty"`
	DependsOn   []string    `yaml:"Depends-On,omitempty" json:",omitempty"`
	User        string      `yaml:"User,omitempty" json:",omitempty"`
	Endpoints   []*Endpoint `yaml:"Endpoints,omitempty" json:",omitempty"`
}

type Endpoint struct {
	PrivateHostName string          `yaml:"Private-Host-Name"`
	PrivatePortName string          `yaml:"Private-Port-Name"`
	PublicHost      string          `yaml:"-" json:"-"`
	ServiceName     string          `yaml:"-" json:"-"`
	PrivateHost     string          `yaml:"-" json:"-"`
	PrivatePort     int32           `yaml:"Private-Port"`
	ProxyMappings   []*ProxyMapping `yaml:"Proxy-Mappings,omitempty" json:",omitempty"`
}

type ProxyMapping struct {
	Frontend  string   `yaml:"Frontend"`
	Backend   string   `yaml:"Backend"`
	Protocols []string `yaml:"Protocols,omitempty" json:"-"`
	Protocol  string   `yaml:"-" json:"Protocol,omitempty"`
}

const ManifestEntry = "manifest/plugin.yml"

func manifestFile(dir string) string {
	return filepath.Join(dir, filepath.FromSlash(ManifestEntry))
}

func IsPluginDir(dir string) bool {
	if strings.HasPrefix(filepath.Base(dir), ".") {
		return false
	} else {
		_, err := os.Stat(manifestFile(dir))
		return err == nil
	}
}

func Load(path string) (*Plugin, error) {
	f, err := os.Open(manifestFile(path))
	if err != nil {
		return nil, err
	}
	defer f.Close()

	plugin, err := Read(f)
	if err != nil {
		return nil, err
	}

	plugin.Path = path
	return plugin, nil
}

func Read(f io.Reader) (*Plugin, error) {
	data, err := ioutil.ReadAll(f)
	if err != nil {
		return nil, err
	}

	plugin := &Plugin{}
	err = yaml.Unmarshal(data, plugin)
	return plugin, err
}

func (p *Plugin) GetEndpoints(publicHost, serviceName, privateHost string) []*Endpoint {
	endpoints := make([]*Endpoint, 0, len(p.Endpoints))

	for _, ep := range p.Endpoints {
		envPrefix := "CLOUDWAY_" + p.Name + "_"
		host_name := strings.ToUpper(envPrefix + ep.PrivateHostName)
		port_name := strings.ToUpper(envPrefix + ep.PrivatePortName)

		ep2 := &Endpoint{
			PrivateHostName: host_name,
			PrivatePortName: port_name,
			PublicHost:      publicHost,
			ServiceName:     serviceName,
			PrivateHost:     privateHost,
			PrivatePort:     ep.PrivatePort,
		}
		ep2.ProxyMappings = proxyMappings(ep2, ep.ProxyMappings)
		endpoints = append(endpoints, ep2)
	}

	return endpoints
}

func proxyMappings(ep *Endpoint, mappings []*ProxyMapping) []*ProxyMapping {
	var res = make([]*ProxyMapping, 0, len(mappings))
	for _, m := range mappings {
		protocols := m.Protocols
		if len(protocols) == 0 {
			protocols = []string{"http"}
		}
		for _, prot := range protocols {
			m2 := ProxyMapping{
				Frontend: getFrontendUri(ep, m.Frontend),
				Backend:  getBackendUri(ep, m.Backend, prot),
				Protocol: prot,
			}
			res = append(res, &m2)
		}
	}
	return res
}

func getFrontendUri(ep *Endpoint, uri string) string {
	if strings.HasSuffix(uri, "/") {
		uri = uri[0 : len(uri)-1]
	}
	if len(uri) != 0 && !strings.HasPrefix(uri, "/") {
		uri = "/" + uri
	}

	host := ep.PublicHost
	if uri != "" && ep.ServiceName != "" && strings.HasPrefix(host, ep.ServiceName+".") {
		host = host[len(ep.ServiceName)+1:]
	}
	return host + uri
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
	return p.Category == Framework
}

func (p *Plugin) IsService() bool {
	return p.Category == Service
}

func (p *Plugin) IsLibrary() bool {
	return p.Category == Library
}

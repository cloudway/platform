package system

import (
	"encoding/json"
	"fmt"
	"io/ioutil"
	"net/http"
	"net/url"
	"strconv"

	"golang.org/x/net/context"
	"gopkg.in/cookieo9/resources-go.v2"
	"gopkg.in/yaml.v2"

	"github.com/cloudway/platform/config/defaults"
)

// Minimalist swagger schema
type swaggerSchema struct {
	Swagger             string      `yaml:"swagger" json:"swagger"`
	Info                interface{} `yaml:"info" json:"info"`
	Schemes             []string    `yaml:"schemes" json:"schemes"`
	Host                string      `yaml:"host" json:"host"`
	BasePath            string      `yaml:"basePath" json:"basePath"`
	Paths               interface{} `yaml:"paths" json:"paths"`
	SecurityDefinitions interface{} `yaml:"securityDefinitions" json:"securityDefinitions"`
	Definitions         interface{} `yaml:"definitions" json:"definitions"`
}

var swaggerJson []byte

func (s *systemRouter) getSwaggerJson(ctx context.Context, w http.ResponseWriter, r *http.Request, vars map[string]string) error {
	if swaggerJson == nil {
		if err := loadSwaggerJson(); err != nil {
			return err
		}
	}

	w.Header().Add("Content-Type", "application/json")
	_, err := w.Write(swaggerJson)
	return err
}

func loadSwaggerJson() error {
	resource, err := resources.Open("swagger.yml")
	if err != nil {
		return err
	}
	defer resource.Close()

	data, err := ioutil.ReadAll(resource)
	if err != nil {
		return err
	}

	var schema swaggerSchema
	err = yaml.Unmarshal(data, &schema)
	if err != nil {
		return err
	}

	// convert map[interface{}]interface{} to map[string]interface{}
	schema.Info = mapValue(schema.Info)
	schema.Paths = mapValue(schema.Paths)
	schema.SecurityDefinitions = mapValue(schema.SecurityDefinitions)
	schema.Definitions = mapValue(schema.Definitions)

	// fill in api server host and scheme
	if baseurl := defaults.ApiURL(); baseurl != "" {
		if u, err := url.Parse(baseurl); err == nil {
			schema.Host = u.Host
			schema.Schemes = []string{u.Scheme}
		}
	}

	swaggerJson, err = json.Marshal(schema)
	return err
}

func mapValue(value interface{}) interface{} {
	switch m := value.(type) {
	case map[interface{}]interface{}:
		ms := make(map[string]interface{})
		for k, v := range m {
			ms[mapKey(k)] = mapValue(v)
		}
		return ms

	case map[string]interface{}:
		for k, v := range m {
			m[k] = mapValue(v)
		}
		return m

	case []interface{}:
		for i, v := range m {
			m[i] = mapValue(v)
		}
		return m

	default:
		return value
	}
}

func mapKey(k interface{}) string {
	switch key := k.(type) {
	case string:
		return key
	case int:
		return strconv.Itoa(key)
	case fmt.Stringer:
		return key.String()
	default:
		panic(fmt.Errorf("cannot convert %t to string", k))
	}
}

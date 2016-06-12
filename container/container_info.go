package container

import (
    "bytes"
    "encoding/json"
    "github.com/cloudway/platform/pkg/manifest"
)

// Get application information from container.
func (c *Container) GetInfo() (*manifest.ApplicationInfo, error) {
    var buf bytes.Buffer
    err := c.ExecE("root", nil, &buf, "/usr/bin/cwctl", "info", "--ip", c.IP())
    if err != nil {
        return nil, err
    }

    var info manifest.ApplicationInfo
    if err = json.NewDecoder(&buf).Decode(&info); err != nil {
        return nil, err
    } else {
        return &info, err
    }
}
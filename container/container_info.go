package container

import (
    "bytes"
    "encoding/json"
    "github.com/cloudway/platform/plugin"
)

// Get application information from container.
func (c *Container) GetInfo() (*plugin.ApplicationInfo, error) {
    var buf bytes.Buffer
    err := c.ExecE("root", nil, &buf, "/usr/bin/cwctl", "info", "--ip", c.IP())
    if err != nil {
        return nil, err
    }

    var info plugin.ApplicationInfo
    if err = json.NewDecoder(&buf).Decode(&info); err != nil {
        return nil, err
    } else {
        return &info, err
    }
}
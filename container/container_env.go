package container

import (
    "bytes"
    "strings"
    "io/ioutil"
    "archive/tar"
    "golang.org/x/net/context"
    "github.com/docker/engine-api/types"
)

// Adds the variable to the environment with the value, if name does not
// exists. If name does exist in the environment, then its value is changed
// to value.
func (c *Container) Setenv(name, value string) error {
    content := []byte(value)

    // Make an archive containing the environmnet file
    buf := &bytes.Buffer{}
    tw := tar.NewWriter(buf)
    tw.WriteHeader(&tar.Header{
        Name: name,
        Mode: 0644,
        Size: int64(len(content)),
    })
    tw.Write(content)
    tw.Close()

    // Copy the archive to the container at specified path
    return c.CopyToContainer(context.Background(), c.ID, c.EnvDir(), buf, types.CopyToContainerOptions{})
}

// Get an environment variable value.
func (c *Container) Getenv(name string) (string, error) {
    // Copy the archive from the container that contains the environment file
    path := c.EnvDir() + "/" + name
    r, _, err := c.CopyFromContainer(context.Background(), c.ID, path)
    if err != nil {
        return "", err
    }
    defer r.Close()

    // Read archive to get environment value
    var content []byte
    tr := tar.NewReader(r)
    if _, err = tr.Next(); err != nil {
        return "", err
    }
    if content, err = ioutil.ReadAll(tr); err != nil {
        return "", err
    }
    return strings.TrimRight(string(content), "\r\n"), nil
}

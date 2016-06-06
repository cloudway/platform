package container

import (
    "fmt"
    "bytes"
    "io"
    "io/ioutil"
    "strings"
    "archive/tar"
    "golang.org/x/net/context"
    "github.com/docker/engine-api/types"
)

// Adds the variable to the environment with the value, if name does not
// exists. If name does exist in the environment, then its value is changed
// to value.
func (c *Container) Setenv(name, value string) error {
    content := []byte(value)

    // Make an archive containing the environemnt file
    buf := new(bytes.Buffer)
    tw := tar.NewWriter(buf)
    tw.WriteHeader(&tar.Header{
        Name:   name,
        Mode:   0644,
        Size:   int64(len(content)),
    })
    tw.Write(content)
    tw.Close()

    // Copy the archive to the container at specified path
    tr := bytes.NewReader(buf.Bytes())
    return c.CopyToContainer(context.Background(), c.ID, c.EnvDir(), tr, types.CopyToContainerOptions{})
}

// Removes the variable from the environment. If the variable does not
// exist in the environemnt then the environment is unchanged.
func (c *Container) Unsetenv(name string) error {
    return c.ExecQ("root", "rm", "-f", c.envfile(name))
}

// Get an environment variable value.
func (c *Container) Getenv(name string) (string, error) {
    env := make(map[string]string)
    if err := c.readenv(env, c.envfile(name)); err != nil {
        return "", err
    }

    if v, ok := env[name]; ok {
        return v, nil
    } else {
        return "", fmt.Errorf("No such environment variable: %s", name)
    }
}

// Load all environment variables from container.
func (c *Container) Environ() (map[string]string, error) {
    env := make(map[string]string)

    // Load user environment variables, ignoring errors
    c.readenv(env, c.RepoDir()+"/.cloudway/env");

    // Merge application environment variables
    if err := c.readenv(env, c.EnvDir()); err != nil {
        return nil, err
    }

    // Merge container environment variables
    for _, ce := range c.Config.Env {
        kv := strings.SplitN(ce, "=", 2)
        env[kv[0]] = kv[1]
    }

    return env, nil
}

func (c *Container) envfile(name string) string {
    return c.EnvDir() + "/" + name
}

func (c *Container) readenv(env map[string]string, path string) error {
    r, _, err := c.CopyFromContainer(context.Background(), c.ID, path)
    if err != nil {
        return err
    }
    defer r.Close()

    tr  := tar.NewReader(r)
    for {
        // advance to next tar entry
        var hdr *tar.Header
        hdr, err = tr.Next()
        if err == io.EOF {
            return nil
        }
        if err != nil {
            return err
        }

        // skip directories
        if hdr.FileInfo().IsDir() {
            continue
        }

        // skip files in subdirectories
        name := strings.TrimPrefix(hdr.Name, ".env/")
        if strings.Contains(name, "/") {
            continue
        }

        // read in environment value
        if b, err := ioutil.ReadAll(tr); err != nil {
            return err
        } else {
            env[name] = strings.TrimRight(string(b), "\r\n")
        }
    }
}

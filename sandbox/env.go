package sandbox

import (
    "fmt"
    "strings"
    "os"
    "io/ioutil"
    "path/filepath"
    "regexp"
    "github.com/Sirupsen/logrus"
    "github.com/cloudway/platform/plugin"
)

var validEnvKey = regexp.MustCompile("^[A-Z_0-9]+$")

func (a *Application) Environ() map[string]string {
    var env = make(map[string]string)

    // load user environment variables
    loadEnv(env, filepath.Join(a.RepoDir(), ".cloudway", "env"))

    // merge application environment variables
    loadEnv(env, a.EnvDir())

    // Merge plugin environemnt variables
    loadPluginsEnv(env, a.HomeDir());

    // Merge system environemnt variables
    for _, e := range os.Environ() {
        kv := strings.SplitN(e, "=", 2)
        env[kv[0]] = kv[1]
    }

    // Collect PATH elements
    collectPathElements(env, "PATH")
    collectPathElements(env, "LD_LIBRARY_PATH")

    return env
}

func loadEnv(env map[string]string, path string) {
    logrus.Debugf("Loading environemnts from %s", path)
    f, err := os.Open(path)
    if err != nil {
        logrus.Debug(err)
        return
    }

    files, err := f.Readdir(0)
    if err != nil {
        logrus.Debug(err)
        return
    }

    for _, fileInfo := range files {
        key := fileInfo.Name()
        if fileInfo.Mode().IsRegular() && validEnvKey.MatchString(key) {
            filename := filepath.Join(path, key)
            logrus.Debugf("Reading env file %s", filename)
            val, err := readEnvFile(filename)
            if err != nil {
                logrus.Debug(err)
            } else {
                logrus.Debugf("ENV: %s=%q", key, val)
                env[key] = val
            }
        }
    }
}

func loadPluginsEnv(env map[string]string, home string) {
    f, err := os.Open(home)
    if err != nil {
        logrus.Debug(err)
        return
    }

    files, err := f.Readdir(0)
    if err != nil {
        logrus.Debug(err)
        return
    }

    for _, subdir := range files {
        path := filepath.Join(home, subdir.Name())
        if subdir.IsDir() && plugin.IsPluginDir(path) {
            loadEnv(env, filepath.Join(path, "env"))
        }
    }
}

func collectPathElements(env map[string]string, varname string) {
    var newenv map[string]string
    if varname == "PATH" {
        // Prevent conflict with the PATH variable
        newenv = make(map[string]string)
        for k, v := range env {
            if !strings.HasSuffix(k, "_LD_LIBRARY_PATH_ELEMENT") {
                newenv[k] = v
            }
        }
    } else {
        newenv = env
    }

    elements := make([]string, 0, len(newenv))
    for k, v := range newenv {
        if strings.HasPrefix(k, "CLOUDWAY_") && strings.HasSuffix(k, "_"+varname+"_ELEMENT") {
            elements = append(elements, v)
        }
    }

    system_path := newenv[varname]
    if system_path != "" {
        elements = append(elements, strings.Split(system_path, string(os.PathListSeparator))...)
    }

    if len(elements) != 0 {
        env[varname] = strings.Join(elements, string(os.PathListSeparator))
    }
}

func (a *Application) envfile(name string) string {
    return filepath.Join(a.EnvDir(), name)
}

func readEnvFile(filename string) (string, error) {
    b, err := ioutil.ReadFile(filename)
    if err != nil {
        return "", err
    }

    val := strings.TrimRight(string(b), "\r\n")
    if strings.ContainsRune(val, 0) { // ignore illegal env vars
        return "", fmt.Errorf("Illegal environment value in %s", filename)
    }
    return val, nil
}

func writeEnvFile(filename, value string) error {
    return ioutil.WriteFile(filename, []byte(value), 0644)
}

func (a *Application) Getenv(name string) string {
    val := os.Getenv(name)
    if val == "" {
        val, _ = readEnvFile(a.envfile(name))
    }
    return val
}

func (a *Application) Setenv(name, value string) error {
    return writeEnvFile(a.envfile(name), value)
}

func (a *Application) Unsetenv(name string) error {
    return os.Remove(a.envfile(name))
}

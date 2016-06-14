package sandbox

import (
    "fmt"
    "strings"
    "os"
    "io/ioutil"
    "path/filepath"
    "regexp"
    "github.com/Sirupsen/logrus"
    "github.com/cloudway/platform/pkg/manifest"
)

var validEnvKey = regexp.MustCompile(`^[A-Z_0-9]+(\.export)?$`)
const exportSuffix = ".export"

func (a *Application) Environ() map[string]string {
    var env = make(map[string]string)

    // load user environment variables
    loadEnv(env, filepath.Join(a.RepoDir(), ".cloudway", "env"), false)

    // merge application environment variables
    loadEnv(env, a.EnvDir(), false)

    // Merge plugin environemnt variables
    loadPluginsEnv(env, a.HomeDir(), false);

    // Merge system environemnt variables
    for _, e := range os.Environ() {
        kv := strings.SplitN(e, "=", 2)
        if strings.HasPrefix(kv[0], "CLOUDWAY_") {
            env[kv[0]] = kv[1]
        } else if _, exists := env[kv[0]]; !exists {
            env[kv[0]] = kv[1]
        }
    }

    // Collect PATH elements
    collectPathElements(env, "PATH")
    collectPathElements(env, "LD_LIBRARY_PATH")

    return env
}

func (a *Application) ExportedEnviron() map[string]string {
    var env = make(map[string]string)

    // load exported application environment variables
    loadEnv(env, a.EnvDir(), true)

    // Merge plugin exported environment variable
    loadPluginsEnv(env, a.HomeDir(), true)

    return env
}

func loadEnv(env map[string]string, path string, exporting bool) {
    logrus.Debugf("Loading environemnts from %s", path)
    files, err := ioutil.ReadDir(path)
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
                continue
            }

            exported := strings.HasSuffix(key, exportSuffix)
            if exported {
                key = key[:len(key)-len(exportSuffix)]
            }
            if exporting && !exported {
                continue
            } else {
                logrus.Debugf("ENV: %s=%q", key, val)
                env[key] = val
            }
        }
    }
}

func loadPluginsEnv(env map[string]string, home string, exporting bool) {
    files, err := ioutil.ReadDir(home)
    if err != nil {
        logrus.Debug(err)
        return
    }

    for _, subdir := range files {
        path := filepath.Join(home, subdir.Name())
        if subdir.IsDir() && manifest.IsPluginDir(path) {
            loadEnv(env, filepath.Join(path, "env"), exporting)
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
    var (
        val string
        err error
        ok  bool
    )
    if val, ok = os.LookupEnv(name); !ok {
        filename := a.envfile(name)
        if val, err = readEnvFile(filename); err != nil {
            val, _ = readEnvFile(filename + exportSuffix)
        }
    }
    return val
}

func (a *Application) Setenv(name, value string, export bool) error {
    filename := a.envfile(name)
    if export {
        filename += exportSuffix
    }
    return writeEnvFile(filename, value)
}

func (a *Application) Unsetenv(name string) {
    filename := a.envfile(name)
    os.Remove(filename)
    os.Remove(filename + exportSuffix)
}

func (a *Application) SetPluginEnv(p *manifest.Plugin, name, value string, export bool) error {
    envdir := filepath.Join(p.Path, "env")
    if err := os.MkdirAll(envdir, 0755); err != nil {
        return err
    }
    filename := filepath.Join(envdir, name)
    if export {
        filename += exportSuffix
    }
    return writeEnvFile(filename, value)
}

func (a *Application) UnsetPluginEnv(p *manifest.Plugin, name string) {
    filename := filepath.Join(p.Path, "env", name)
    os.Remove(filename)
    os.Remove(filename + exportSuffix)
}

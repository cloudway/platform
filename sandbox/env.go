package sandbox

import (
    "fmt"
    "strings"
    "os"
    "io/ioutil"
    "path/filepath"
    "regexp"
    "strconv"
    "reflect"
    "unsafe"
    "syscall"
    "github.com/Sirupsen/logrus"
    "github.com/cloudway/platform/pkg/manifest"
)

var validEnvKey = regexp.MustCompile(`^[A-Z_0-9]+(\.export)?$`)
const exportSuffix = ".export"

func (box *Sandbox) Environ() map[string]string {
    var env = make(map[string]string)

    // load user environment variables
    loadEnv(env, filepath.Join(box.RepoDir(), ".cloudway", "env"), false)

    // merge application environment variables
    loadEnv(env, box.EnvDir(), false)

    // Merge plugin environemnt variables
    loadPluginsEnv(env, box.HomeDir(), false);

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

func (box *Sandbox) ExportedEnviron() map[string]string {
    var env = make(map[string]string)

    // load exported application environment variables
    loadEnv(env, box.EnvDir(), true)

    // Merge plugin exported environment variable
    loadPluginsEnv(env, box.HomeDir(), true)

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

func (box *Sandbox) envfile(name string) string {
    return filepath.Join(box.EnvDir(), name)
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

func (box *Sandbox) Getenv(name string) string {
    var (
        val string
        err error
        ok  bool
    )
    if val, ok = os.LookupEnv(name); !ok {
        filename := box.envfile(name)
        if val, err = readEnvFile(filename); err != nil {
            val, _ = readEnvFile(filename + exportSuffix)
        }
    }
    return val
}

func (box *Sandbox) Setenv(name, value string, export bool) error {
    filename := box.envfile(name)
    if export {
        filename += exportSuffix
    }
    return writeEnvFile(filename, value)
}

func (box *Sandbox) Unsetenv(name string) {
    filename := box.envfile(name)
    os.Remove(filename)
    os.Remove(filename + exportSuffix)
}

func (box *Sandbox) SetPluginEnv(p *manifest.Plugin, name, value string, export bool) error {
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

func (box *Sandbox) UnsetPluginEnv(p *manifest.Plugin, name string) {
    filename := filepath.Join(p.Path, "env", name)
    os.Remove(filename)
    os.Remove(filename + exportSuffix)
}

func (box *Sandbox) ActiveState() manifest.ActiveState {
    str, err := readEnvFile(box.envfile(".state"))
    if err != nil {
        return manifest.StateNew
    }
    i, err := strconv.Atoi(str)
    if err != nil {
        return manifest.StateUnknown
    }
    return manifest.ActiveState(i)
}

func (box *Sandbox) SetActiveState(s manifest.ActiveState) {
    writeEnvFile(box.envfile(".state"), strconv.Itoa(int(s)))
    UpdateActiveState(s)
}

func UpdateActiveState(state manifest.ActiveState) {
    if os.Getpid() == 1 {
        // When we are running in docker container as PID 1, we can change
        // our argv to expose the state value. Utilities like 'ps' can get
        // the state value from argv.
        if len(os.Args) < 2 || len(os.Args[1]) < 3 {
            panic("cannot change argv")
        }

        argv1str := (*reflect.StringHeader)(unsafe.Pointer(&os.Args[1]))
        argv1 := (*[1<<30]byte)(unsafe.Pointer(argv1str.Data))[:argv1str.Len]
        copy(argv1, fmt.Sprintf("[%d]", state))
    } else {
        // Otherwise, signal PID 1 to change its state.
        syscall.Kill(1, syscall.SIGUSR1)
    }
}

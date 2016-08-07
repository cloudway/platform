package config

import (
    "os"
    "io"
    "bytes"
    "strings"
    "errors"
    "runtime"
    "path/filepath"
    "github.com/cloudway/platform/pkg/conf"
)

// The root directory of cloudway installation.
var RootDir string

// The global configuration file
var cfg *conf.ConfigFile

// The configuration file name
var cfgFile string

// Initializes the global configuration file.
func Initialize() (err error) {
    // Determine the root installation directory
    root := os.Getenv("CLOUDWAY_ROOT")
    if root == "" {
        root = "/usr/local/cloudway" // the default root
    }
    root, err = filepath.Abs(root)
    if err != nil {
        return err
    }

    // Load configuration file
    path := filepath.Join(root, "conf", "cloudway.conf")
    cfg, err = conf.ReadConfigFile(path)
    if err != nil && !os.IsNotExist(err) { // Use defaults if configuration file is missing
        return err
    }

    RootDir = root
    cfgFile = path
    return nil
}

// Initialize the client configuration file.
func InitializeClient() (err error) {
    home := os.Getenv("HOME")
    if home == "" && runtime.GOOS == "windows" {
        home = os.Getenv("USERPROFILE")
    }
    if home == "" {
        return errors.New("Cannot locate home directory")
    }

    path := filepath.Join(home, ".cloudway")
    cfg, err = conf.ReadConfigFile(path)
    if err != nil && !os.IsNotExist(err) {
        return err
    }

    cfgFile = path
    return nil
}

// Save configurations to file.
func Save() (err error) {
    if cfg == nil && cfgFile == "" {
        return errors.New("the configuration was not initialized")
    }

    if err := os.MkdirAll(filepath.Dir(cfgFile), 0750); err != nil {
        return err
    }

    var file *os.File
    if file, err = os.Create(cfgFile); err != nil {
        return err
    }
    defer file.Close()

    if err = writeSection(file, conf.DefaultSection); err != nil {
        return err
    }
    for _, section := range cfg.GetSections() {
        if section != conf.DefaultSection {
            if err = writeSection(file, section); err != nil {
                return err
            }
        }
    }
    return nil
}

func writeSection(w io.Writer, section string) (err error) {
    buf := bytes.Buffer{}

    if section != conf.DefaultSection {
        buf.WriteString("[" + section + "]\n")
    }
    for key, value := range GetSection(section) {
        buf.WriteString(key + " = " + value + "\n")
    }
    buf.WriteString("\n")

    _, err = buf.WriteTo(w)
    return err
}

// Get a configuration value as string.
func Get(key string) string {
    return GetOrDefault(key, "")
}

// Get a configuration value, if no such value configured then the default value is returned.
func GetOrDefault(key, deflt string) string {
    // get value from environment
    envKey := "CLOUDWAY_" + strings.ToUpper(key)
    envKey  = strings.Replace(envKey, "-", "_", -1)
    envKey  = strings.Replace(envKey, ".", "_", -1)
    if envValue, ok := os.LookupEnv(envKey); ok {
        return envValue
    }

    // get value from configuration file
    if cfg != nil {
        section := conf.DefaultSection
        parts := strings.SplitN(key, ".", 2)
        if len(parts) == 2 {
            section, key = parts[0], parts[1]
        }
        if value, err := cfg.GetString(section, key); err == nil {
            return value
        }
    }

    // return the default value
    return deflt
}

// Get a section in the configuration file.
func GetSection(section string) map[string]string {
    result := make(map[string]string)

    if cfg == nil {
        return result
    }

    options, err := cfg.GetOptions(section)
    if err != nil {
        return result
    }

    for _, opt := range options {
        value, err := cfg.GetString(section, opt)
        if err == nil {
            result[opt] = value
        }
    }

    return result
}

// Set a configuration of the given key to the given value.
func Set(key, value string) {
    if cfg == nil {
        cfg = conf.NewConfigFile()
    }

    section := conf.DefaultSection
    parts := strings.SplitN(key, ".", 2)
    if len(parts) == 2 {
        section, key = parts[0], parts[1]
    }
    cfg.AddOption(section, key, value)
}

// Remove a key from configuration.
func Remove(key string) {
    if cfg == nil {
        return
    }

    section := conf.DefaultSection
    parts := strings.SplitN(key, ".", 2)
    if len(parts) == 2 {
        section, key = parts[0], parts[1]
    }
    cfg.RemoveOption(section, key)
}

// Remove a section from configuration.
func RemoveSection(section string) {
    if cfg != nil {
        cfg.RemoveSection(section)
    }
}

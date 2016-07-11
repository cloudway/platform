package conf

import (
    "os"
    "io"
    "bytes"
    "strings"
    "errors"
    "path/filepath"
    "github.com/cloudway/platform/pkg/conf"
)

// The root directory of cloudway installation.
var RootDir string

// The global configuration file
var cfg *conf.ConfigFile

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
    RootDir = root

    // Load configuration file
    cfgFile := filepath.Join(RootDir, "conf", "cloudway.conf")
    cfg, err = conf.ReadConfigFile(cfgFile)
    if err != nil && !os.IsNotExist(err) { // Use defaults if configuration file is missing
        return err
    }
    return nil
}

// Save configurations to file.
func Save() (err error) {
    if cfg == nil || RootDir == "" {
        return errors.New("the configuration was not initialized")
    }

    var file *os.File
    fname := filepath.Join(RootDir, "conf", "cloudway.conf")
    if file, err = os.Create(fname); err != nil {
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

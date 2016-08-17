package config

import (
    "os"
    "runtime"
    "errors"
    "strings"
    "path/filepath"
)

// The root directory of cloudway installation.
var RootDir string

// The global configuration file
var global *Config

// Error is paniked if the global configuration was not initialized.
var ErrNotInitialized = errors.New("the configuration was not initialized")

// Initializes the global configuration file.
func Initialize() (err error) {
    // Determine the root installation directory
    RootDir = os.Getenv("CLOUDWAY_ROOT")
    if RootDir == "" {
        RootDir = "/usr/local/cloudway" // the default root
    }
    RootDir, err = filepath.Abs(RootDir)
    if err != nil {
        return err
    }

    // Load configuration file
    filename := filepath.Join(RootDir, "conf", "cloudway.conf")
    global, err = Open(filename)
    if err != nil && !os.IsNotExist(err) { // Use defaults if configuration file is missing
        return err
    }
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

    filename := filepath.Join(home, ".cloudway")
    global, err = Open(filename)
    if err != nil && !os.IsNotExist(err) {
        return err
    }
    return nil
}

// Save global configurations to file.
func Save() (err error) {
    if global == nil {
        return ErrNotInitialized
    }
    return global.Save()
}

// Get a configuration value as string
func Get(key string) string {
    return GetOrDefault(key, "")
}

// GetOrDefault get a configuration value, if no such value configured then
// the default value is returned.
func GetOrDefault(key, deflt string) string {
    // get value from environment
    envKey := "CLOUDWAY_" + strings.ToUpper(key)
    envKey  = strings.Replace(envKey, "-", "_", -1)
    envKey  = strings.Replace(envKey, ".", "_", -1)
    if envValue, ok := os.LookupEnv(envKey); ok {
        return envValue
    }

    // get value from configuration file
    if global != nil {
        return global.GetOrDefault(key, deflt)
    }

    // return the default value
    return deflt
}

// Set a configuration of the given key to the given value.
func Set(key, value string) {
    if global == nil {
        panic(ErrNotInitialized)
    }
    global.Set(key, value)
}

// Remove a key from configuration.
func Remove(key string) {
    if global == nil {
        panic(ErrNotInitialized)
    }
    global.Remove(key)
}

// GetSections returns the list of sections in the configuration.
func GetSections() []string {
    if global == nil {
        panic(ErrNotInitialized)
    }
    return global.GetSections()
}

// GetSection get section in the configuration file.
func GetSection(section string) map[string]string {
    if global == nil {
        panic(ErrNotInitialized)
    }
    return global.GetSection(section)
}

// RemoveSection remove a section from configuration.
func RemoveSection(section string) {
    if global == nil {
        panic(ErrNotInitialized)
    }
    global.RemoveSection(section)
}

// GetFrom get a configuration value from the given section.
func GetOption(section, key string) string {
    if global == nil {
        panic(ErrNotInitialized)
    }
    return global.GetOption(section, key)
}

// AddOption add a configuration value into the given section.
func AddOption(section, key, value string) {
    if global == nil {
        panic(ErrNotInitialized)
    }
    global.AddOption(section, key, value)
}

// RemoveOption removes a configuration value from the given section.
func RemoveOption(section, key string) {
    if global == nil {
        panic(ErrNotInitialized)
    }
    global.RemoveOption(section, key)
}

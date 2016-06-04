package conf

import (
    "os"
    "path/filepath"
    "github.com/spf13/viper"
)

// The root directory of cloudway installation.
var RootDir string

const defaultConfigType = "toml"

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
    viper.AutomaticEnv()
    viper.SetConfigFile(cfgFile)
    viper.SetConfigType(defaultConfigType)

    err = viper.ReadInConfig()
    if err != nil {
        // Use defaults if configuration file is missing
        if _, ok := err.(viper.ConfigParseError); ok {
            return err
        }
    }

    return nil
}

// Load a configuration file.
func Load(name string) (*viper.Viper, error) {
    cfgType := filepath.Ext(name)
    if cfgType == "" {
        name += ".conf"
        cfgType = defaultConfigType
    } else {
        cfgType = cfgType[1:]
    }

    cfgFile := filepath.Join(RootDir, "conf", name)

    conf := viper.New()
    conf.SetConfigFile(cfgFile)
    conf.SetConfigType(cfgType)
    return conf, conf.ReadInConfig()
}

func GetOrDefault(key, deflt string) string {
    if viper.IsSet(key) {
        return viper.GetString(key)
    } else {
        return deflt
    }
}

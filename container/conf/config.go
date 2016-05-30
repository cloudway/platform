package conf

import (
    "os"
    "path/filepath"
    "github.com/spf13/viper"
)

// The home directory of cloudway installation.
var HomeDir string

const defaultConfigType = "toml"

// Initializes the global configuration file.
func Initialize() (err error) {
    // Determine the home installation directory
    home := os.Getenv("CLOUDWAY_HOME")
    if home == "" {
        home = "/usr/local/cloudway" // the default home
    }
    home, err = filepath.Abs(home)
    if err != nil {
        return err
    }
    HomeDir = home

    // Load configuration file
    cfgFile := filepath.Join(HomeDir, "conf/cloudway.conf")
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

    cfgFile := filepath.Join(HomeDir, "conf", name)

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

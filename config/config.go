package config

import (
    "os"
    "io"
    "bytes"
    "strings"
    "path/filepath"
    "github.com/cloudway/platform/pkg/conf"
)

type Config struct {
    filename string
    cfg *conf.ConfigFile
}

// Create a new configuration.
func New(filename string) *Config {
    return &Config{filename: filename, cfg: conf.NewConfigFile()}
}

// Open Configuration file from the give file name.
func Open(filename string) (*Config, error) {
    cfg, err := conf.ReadConfigFile(filename)
    return &Config{
        filename: filename,
        cfg:      cfg,
    }, err
}

// Save configurations to file.
func (c *Config) Save() (err error) {
    if err := os.MkdirAll(filepath.Dir(c.filename), 0750); err != nil {
        return err
    }

    var file *os.File
    if file, err = os.Create(c.filename); err != nil {
        return err
    }
    defer file.Close()

    if err = c.writeSection(file, conf.DefaultSection); err != nil {
        return err
    }
    for _, section := range c.GetSections() {
        if section != conf.DefaultSection {
            if err = c.writeSection(file, section); err != nil {
                return err
            }
        }
    }
    return nil
}

func (c *Config) writeSection(w io.Writer, section string) (err error) {
    options := c.GetSection(section)
    if len(options) == 0 {
        return
    }

    buf := bytes.Buffer{}

    if section != conf.DefaultSection {
        buf.WriteString("[" + section + "]\n")
    }
    for key, value := range options {
        buf.WriteString(key + " = " + value + "\n")
    }
    buf.WriteString("\n")

    _, err = buf.WriteTo(w)
    return err
}

// Get a configuration value as string.
func (c *Config) Get(key string) string {
    return c.GetOrDefault(key, "")
}

// GetOrDefault get a configuration value, if no such value configured then
// the default value is returned.
func (c *Config) GetOrDefault(key, deflt string) string {
    if c.cfg != nil {
        section := conf.DefaultSection
        parts := strings.SplitN(key, ".", 2)
        if len(parts) == 2 {
            section, key = parts[0], parts[1]
        }
        if value, err := c.cfg.GetString(section, key); err == nil {
            return value
        }
    }
    return deflt
}

// Set a configuration of the given key to the given value.
func (c *Config) Set(key, value string) {
    if c.cfg == nil {
        c.cfg = conf.NewConfigFile()
    }

    section := conf.DefaultSection
    parts := strings.SplitN(key, ".", 2)
    if len(parts) == 2 {
        section, key = parts[0], parts[1]
    }
    c.cfg.AddOption(section, key, value)
}

// Remove a key from configuration.
func (c *Config) Remove(key string) {
    if c.cfg == nil {
        return
    }

    section := conf.DefaultSection
    parts := strings.SplitN(key, ".", 2)
    if len(parts) == 2 {
        section, key = parts[0], parts[1]
    }
    c.cfg.RemoveOption(section, key)
}

// GetSections returns the list of sections in the configuration.
func (c *Config) GetSections() []string {
    if c.cfg == nil {
        return make([]string, 0)
    } else {
        return c.cfg.GetSections()
    }
}

// GetSection get a section in the configuration file.
func (c *Config) GetSection(section string) map[string]string {
    result := make(map[string]string)

    if c.cfg == nil {
        return result
    }

    options, err := c.cfg.GetOptions(section)
    if err != nil {
        return result
    }

    for _, opt := range options {
        value, err := c.cfg.GetString(section, opt)
        if err == nil {
            result[opt] = value
        }
    }

    return result
}

// RemoveSection remove a section from configuration.
func (c *Config) RemoveSection(section string) {
    if c.cfg != nil {
        c.cfg.RemoveSection(section)
    }
}

// GetFrom get a configuration value from the given section.
func (c *Config) GetOption(section, key string) (value string) {
    if c.cfg != nil {
        value, _ = c.cfg.GetString(section, key)
    }
    return value
}

// AddOption add a configuration value into the given section.
func (c *Config) AddOption(section, key, value string) {
    if c.cfg == nil {
        c.cfg = conf.NewConfigFile()
    }
    c.cfg.AddOption(section, key, value)
}

// RemoveFrom removes a configuration value from the given section.
func (c *Config) RemoveOption(section, key string) {
    if c.cfg != nil {
        c.cfg.RemoveOption(section, key)
    }
}

package defaults

import "github.com/cloudway/platform/container/conf"

func Domain() string {
    return conf.GetOrDefault("domain", "cloudway.local")
}

func AppHome() string {
    return conf.GetOrDefault("app-home", "/app")
}

func AppUser() string {
    return conf.GetOrDefault("app-user", "cwuser")
}

func AppCapacity() string {
    return conf.GetOrDefault("app-capacity", "small")
}

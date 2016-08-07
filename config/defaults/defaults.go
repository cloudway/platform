package defaults

import "github.com/cloudway/platform/config"

func Domain() string {
    return config.GetOrDefault("domain", "cloudway.local")
}

func AppHome() string {
    return config.GetOrDefault("app-home", "/app")
}

func AppUser() string {
    return config.GetOrDefault("app-user", "cwuser")
}

func AppCapacity() string {
    return config.GetOrDefault("app-capacity", "small")
}

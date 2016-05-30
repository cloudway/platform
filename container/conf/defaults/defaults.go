package defaults

import "icloudway.com/platform/container/conf"

func Domain() string {
    return conf.GetOrDefault("CLOUDWAY_DOMAIN", "cloudway.local")
}

func AppHome() string {
    return conf.GetOrDefault("CLOUDWAY_APP_HOME", "/cloudway")
}

func AppUser() string {
    return conf.GetOrDefault("CLOUDWAY_APP_USER", "cwuser")
}

func AppCapacity() string {
    return conf.GetOrDefault("CLOUDWAY_APP_CAPACITY", "small")
}

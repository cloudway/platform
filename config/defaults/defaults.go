package defaults

import "github.com/cloudway/platform/config"

func Domain() string {
	return config.GetOrDefault("domain", "cloudway.local")
}

func ConsoleURL() string {
	// Don't confuse.  The console and API server are combined in a single
	// service by default, but you can also separate them into two services.
	return config.GetOrDefault("console.url", "http://api."+Domain())
}

func ApiURL() (url string) {
	if url = config.Get("api.url"); url == "" {
		url = ConsoleURL()
	}
	return
}

func AppHome() string {
	return config.GetOrDefault("app-home", "/app")
}

func AppUser() string {
	return config.GetOrDefault("app-user", "cwuser")
}

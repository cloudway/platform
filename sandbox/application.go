package sandbox

import (
    "os"
    "os/user"
    "strconv"
    "path/filepath"
    "github.com/Sirupsen/logrus"
    "github.com/cloudway/platform/plugin"
)

type Application struct {
    name        string
    namespace   string
    home        string
    user        string
    uid         int
    gid         int
}

func NewApplication() *Application {
    var (
        name      = os.Getenv("CLOUDWAY_APP_NAME")
        namespace = os.Getenv("CLOUDWAY_APP_NAMESPACE")
        home      = os.Getenv("CLOUDWAY_HOME_DIR")
        username  = os.Getenv("CLOUDWAY_APP_USER")
    )

    if home == "" || name == "" || namespace == "" {
        logrus.Fatal("Invalid application environment")
    }

    uid, gid := 0, 0
    if username != "" {
        if u, err := user.Lookup(username); err != nil {
            logrus.Error(err)
        } else {
            uid, _ = strconv.Atoi(u.Uid)
            gid, _ = strconv.Atoi(u.Gid)
        }
    }

    return &Application{
        name:       name,
        namespace:  namespace,
        home:       home,
        user:       username,
        uid:        uid,
        gid:        gid,
    }
}

func (app *Application) HomeDir() string {
    return app.home
}

func (app *Application) EnvDir() string {
    return filepath.Join(app.home, ".env")
}

func (app *Application) RepoDir() string {
    return filepath.Join(app.HomeDir(), "repo")
}

func (app *Application) DataDir() string {
    return filepath.Join(app.HomeDir(), "data")
}

func (app *Application) Name() string {
    return app.name
}

func (app *Application) Namespace() string {
    return app.namespace
}

func (app *Application) FQDN() string {
    return os.Getenv("CLOUDWAY_APP_DNS")
}

func (app *Application) GetPlugins() (map[string]*plugin.Plugin, error) {
    d, err := os.Open(app.HomeDir())
    if err != nil {
        return nil, err
    }

    names, err := d.Readdirnames(-1)
    if err != nil {
        return nil, err
    }

    plugins := make(map[string]*plugin.Plugin)
    for _, filename := range names {
        subdir := filepath.Join(app.HomeDir(), filename)
        if plugin.IsPluginDir(subdir) {
            p, err := plugin.Load(subdir)
            if err != nil {
                logrus.WithError(err).Errorf("Faield to load plugin %s", filename)
            } else {
                plugins[p.Name] = p
            }
        }
    }
    return plugins, nil
}

func (app *Application) GetPlugin(name string) *plugin.Plugin {
    plugins, err := app.GetPlugins()
    if err == nil {
        return plugins[name]
    } else {
        return nil
    }
}

func (app *Application) GetFrameworkPlugin() *plugin.Plugin {
    plugins, err := app.GetPlugins()
    if err == nil {
        for _, p := range plugins {
            if p.IsFramework() {
                return p
            }
        }
    }
    return nil
}

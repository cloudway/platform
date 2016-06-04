package sandbox

import (
    "os"
    "path/filepath"
    "github.com/Sirupsen/logrus"
)

type Application struct {
    home        string
    name        string
    namespace   string
}

func NewApplication() *Application {
    var (
        home      = os.Getenv("CLOUDWAY_HOME_DIR")
        name      = os.Getenv("CLOUDWAY_APP_NAME")
        namespace = os.Getenv("CLOUDWAY_APP_NAMESPACE")
    )

    if home == "" || name == "" || namespace == "" {
        logrus.Fatal("Invalid application environment")
    }

    return &Application{
        home:       home,
        name:       name,
        namespace:  namespace,
    }
}

func (app *Application) HomeDir() string {
    return app.home
}

func (app *Application) EnvDir() string {
    return filepath.Join(app.home, ".env")
}

func (app *Application) AppDir() string {
    return filepath.Join(app.home, "app")
}

func (app *Application) RepoDir() string {
    return filepath.Join(app.AppDir(), "repo")
}

func (app *Application) DataDir() string {
    return filepath.Join(app.AppDir(), "data")
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


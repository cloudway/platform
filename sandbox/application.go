package sandbox

import (
    "os"
    "os/user"
    "strconv"
    "path/filepath"
    "github.com/Sirupsen/logrus"
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

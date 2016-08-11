package cmds

import (
    "os"
    "errors"
    "strings"
    "os/exec"
    "path/filepath"
    "github.com/cloudway/platform/api/types"
)

func gitClone(host string, app *types.ApplicationInfo) error {
    if app.CloneURL == "" {
        return errors.New("Cannot determine the clone command")
    }

    args := strings.Fields(app.CloneURL)
    cmd := exec.Command(args[0], args[1:]...)
    cmd.Stdout = os.Stdout
    cmd.Stderr = os.Stderr
    if err := cmd.Run(); err != nil {
        return err
    }

    if app.SCMType == "git" {
        gitSetConfig(app.Name, "cloudway.host", host)
        gitSetConfig(app.Name, "cloudway.app", app.Name)
    }

    return nil
}

func gitSetConfig(path, name, value string) error {
    config := filepath.Join(path, ".git", "config")
    return exec.Command("git", "config", "-f", config, name, value).Run()
}

func gitGetConfig(name string) string {
    out, err := exec.Command("git", "config", "--get", name).Output()
    if err != nil {
        return ""
    }
    return strings.TrimSpace(string(out))
}

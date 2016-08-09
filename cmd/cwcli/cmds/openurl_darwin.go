package cmds

import "os/exec"

func openurl(url string) error {
    cmd := exec.Command("open", url)
    return cmd.Run()
}

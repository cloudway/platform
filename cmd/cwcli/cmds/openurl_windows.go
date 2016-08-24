package cmds

import "os/exec"

func openurl(url string) error {
	cmd := exec.Command("cmd.exe", "/c", "start "+url)
	return cmd.Run()
}

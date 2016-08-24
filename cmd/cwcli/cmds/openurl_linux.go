package cmds

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
)

const shellScript = `
URL="%s"
[ -x "$BROWSER" ] && exec "$BROWSER" $URL
command -v xdg-open &>/dev/null && exec xdg-open $URL
command -v gnome-open &>/dev/null && exec gnome-open $URL
command -v python &>/dev/null && exec python -m webbrowser $URL
echo "Cannot find brower to open $URL"
`

func openurl(url string) error {
	script := fmt.Sprintf(shellScript, url)
	cmd := exec.Command("bash")
	cmd.Stdin = bytes.NewReader([]byte(script))
	cmd.Stdout = os.Stdout
	return cmd.Run()
}

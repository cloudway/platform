package cmds

import (
    "os"
    "golang.org/x/crypto/ssh/terminal"
)

var alert, hilite func(text string) string

func init() {
    if terminal.IsTerminal(int(os.Stdout.Fd())) {
        alert = func(text string) string {
            return "\033[31;1m" + text + "\033[0m"
        }
        hilite = func(text string) string {
            return "\033[1m" + text + "\033[0m"
        }
    } else {
        id := func(text string) string {
            return text
        }
        alert = id
        hilite = id
    }
}

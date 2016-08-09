// +build !windows,!darwin,!linux

package cmds

import "fmt"

func openurl(url string) error {
    fmt.Printf("Cannot find browser to open %s\n", url)
    return nil
}

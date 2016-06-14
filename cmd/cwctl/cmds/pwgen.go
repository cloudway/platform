package cmds

import (
    "os"
    "regexp"
    "bytes"
    "crypto/rand"
    "github.com/cloudway/platform/pkg/mflag"
)

// Utility command to generate a random password
func (cli *CWCtl) CmdPwgen(args ...string) error {
    cmd := cli.Subcmd("pwgen")
    pwLength := cmd.Int([]string{"l", "-length"}, 12, "Length of the generated password")
    pwRange  := cmd.String([]string{"r", "-range"}, "a-np-zA-NP-Z1-9", "Range of characters to generate")
    cmd.Require(mflag.Max, 1)
    cmd.ParseFlags(args, true)

    pwPrefix := ""
    if cmd.NArg() == 1 {
        pwPrefix = cmd.Arg(0)
    }

    re, err := regexp.Compile("[^" + *pwRange + "]")
    if err != nil {
        return err
    }

    buf := bytes.Buffer{}
    buf.WriteString(pwPrefix)
    for buf.Len() < *pwLength {
        var b [128]byte
        n, err := rand.Read(b[:])
        if err != nil {
            return err
        }
        buf.Write(re.ReplaceAll(b[:n], []byte{}))
    }

    buf.Truncate(*pwLength)
    _, err = buf.WriteTo(os.Stdout)
    return err
}

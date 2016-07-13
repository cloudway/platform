package cmds

import "github.com/cloudway/platform/sshd"

func (cli *CWMan) CmdSshd(args ...string) error {
    var addr string

    cmd := cli.Subcmd("sshd")
    cmd.StringVar(&addr, []string{"-bind"}, "0.0.0.0:2200", "SSHD bind address")
    cmd.ParseFlags(args, true)

    return sshd.Serve(addr)
}

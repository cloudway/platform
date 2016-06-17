package cmds

import (
    "github.com/cloudway/platform/pkg/mflag"
    "github.com/cloudway/platform/api/server/auth/user"
)

func (cli *CWMan) CmdUserAdd(args ...string) error {
    cmd := cli.Subcmd("useradd", "USERNAME PASSWORD NAMESPACE")
    cmd.Require(mflag.Exact, 3)
    cmd.ParseFlags(args, true)

    userdb, err := user.OpenUserDatabase()
    if err != nil {
        return err
    }

    return userdb.Create(&user.User{
        Name:       cmd.Arg(0),
        Password:   []byte(cmd.Arg(1)),
        Namespace:  cmd.Arg(2),
    })
}

func (cli *CWMan) CmdUserDel(args ...string) error {
    cmd := cli.Subcmd("userdel", "USERNAME")
    cmd.Require(mflag.Exact, 1)
    cmd.ParseFlags(args, true)

    userdb, err := user.OpenUserDatabase()
    if err != nil {
        return err
    }
    return userdb.Remove(cmd.Arg(0))
}

package cmds

import (
    "github.com/cloudway/platform/pkg/mflag"
    "github.com/cloudway/platform/auth/userdb"
    "github.com/cloudway/platform/broker"
    "github.com/cloudway/platform/config/defaults"
)

type CustomUser struct {
    userdb.BasicUser `bson:",inline"`
    Email string
}

func (cli *CWMan) CmdUserAdd(args ...string) (err error) {
    cmd := cli.Subcmd("useradd", "USERNAME PASSWORD [NAMESPACE]")
    cmd.Require(mflag.Min, 2)
    cmd.Require(mflag.Max, 3)
    cmd.ParseFlags(args, true)

    br, err := broker.New(cli.DockerClient)
    if err != nil {
        return err
    }

    user := &CustomUser{}
    user.Name = cmd.Arg(0)
    user.Email = user.Name + "@" + defaults.Domain()
    if cmd.NArg() == 3 {
        user.Namespace = cmd.Arg(2)
    }
    return br.CreateUser(user, cmd.Arg(1))
}

func (cli *CWMan) CmdUserDel(args ...string) error {
    cmd := cli.Subcmd("userdel", "USERNAME")
    cmd.Require(mflag.Exact, 1)
    cmd.ParseFlags(args, true)

    br, err := broker.New(cli.DockerClient)
    if err != nil {
        return err
    }
    return br.RemoveUser(cmd.Arg(0))
}

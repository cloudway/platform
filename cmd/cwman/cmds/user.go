package cmds

import (
    "github.com/cloudway/platform/pkg/mflag"
    "github.com/cloudway/platform/pkg/opts"
    "github.com/cloudway/platform/auth/userdb"
)

type CustomUser struct {
    userdb.BasicUser `bson:",inline"`
    Email string
}

func (cli *CWMan) CmdUserAdd(args ...string) error {
    cmd := cli.Subcmd("useradd", "USERNAME PASSWORD NAMESPACE")
    cmd.Require(mflag.Exact, 3)
    cmd.ParseFlags(args, true)

    db, err := userdb.Open()
    if err != nil {
        return err
    }

    user := &CustomUser{
        BasicUser: userdb.BasicUser {
            Name:       cmd.Arg(0),
            Namespace:  cmd.Arg(2),
        },
        Email: cmd.Arg(0) + "@example.com",
    }

    return db.Create(user, cmd.Arg(1))
}

func (cli *CWMan) CmdUserDel(args ...string) error {
    cmd := cli.Subcmd("userdel", "USERNAME")
    cmd.Require(mflag.Exact, 1)
    cmd.ParseFlags(args, true)

    db, err := userdb.Open()
    if err != nil {
        return err
    }
    return db.Remove(cmd.Arg(0))
}

func (cli *CWMan) CmdUserMod(args ...string) error {
    fields := make(map[string]string)

    cmd := cli.Subcmd("usermod NAME")
    cmd.Var(opts.NewMapOptsRef(&fields, nil), []string{"f"}, "Update fields")
    cmd.Require(mflag.Exact, 1)
    cmd.ParseFlags(args, true)

    db, err := userdb.Open()
    if err != nil {
        return err
    }

    delete(fields, "name")
    delete(fields, "namespace")
    delete(fields, "password")

    return db.Update(cmd.Arg(0), fields)
}

func (cli *CWMan) CmdPassword(args ...string) error {
    cmd := cli.Subcmd("password", "USER OLD_PASSWORD NEW_PASSWORD")
    cmd.Require(mflag.Exact, 3)
    cmd.ParseFlags(args, true)

    db, err := userdb.Open()
    if err != nil {
        return err
    }
    return db.ChangePassword(cmd.Arg(0), cmd.Arg(1), cmd.Arg(2))
}

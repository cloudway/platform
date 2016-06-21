package cmds

import (
    "github.com/cloudway/platform/pkg/mflag"
    "github.com/cloudway/platform/pkg/opts"
    "github.com/cloudway/platform/pkg/errors"
    "github.com/cloudway/platform/auth/userdb"
    "github.com/cloudway/platform/api/server/runtime"
)

type CustomUser struct {
    userdb.BasicUser `bson:",inline"`
    Email string
}

func (cli *CWMan) CmdUserAdd(args ...string) (err error) {
    cmd := cli.Subcmd("useradd", "USERNAME PASSWORD NAMESPACE")
    cmd.Require(mflag.Exact, 3)
    cmd.ParseFlags(args, true)

    rt, err := runtime.New(cli.DockerClient)
    if err != nil {
        return err
    }

    // create the user in the database
    user := &CustomUser{
        BasicUser: userdb.BasicUser {
            Name:       cmd.Arg(0),
            Namespace:  cmd.Arg(2),
        },
        Email: cmd.Arg(0) + "@example.com",
    }
    err = rt.Users.Create(user, cmd.Arg(1))
    if err != nil {
        return err
    }

    // create the namespace in the SCM
    err = rt.SCM.CreateNamespace(user.Namespace)
    if err != nil {
        rt.Users.Remove(user.Name)
        return err
    }

    return nil
}

func (cli *CWMan) CmdUserDel(args ...string) error {
    cmd := cli.Subcmd("userdel", "USERNAME")
    cmd.Require(mflag.Exact, 1)
    cmd.ParseFlags(args, true)

    rt, err := runtime.New(cli.DockerClient)
    if err != nil {
        return err
    }

    var user userdb.BasicUser
    err = rt.Users.Find(cmd.Arg(0), &user)
    if err != nil {
        return err
    }

    var errors errors.Errors

    // remove all containers belongs to the user
    cs, err := cli.FindAll("", user.Namespace)
    if err == nil {
        for _, c := range cs {
            errors.Add(c.Destroy())
        }
    } else {
        errors.Add(err)
    }

    // remove the uesr namespace from SCM
    errors.Add(rt.SCM.RemoveNamespace(user.Namespace))

    // remove user from user database
    errors.Add(rt.Users.Remove(user.Name))

    if errors.Len() != 0 {
        return errors
    } else {
        return nil
    }
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

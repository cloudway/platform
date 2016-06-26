package cmds

import (
    "fmt"
    "io/ioutil"
    "github.com/cloudway/platform/pkg/mflag"
    "github.com/cloudway/platform/scm"
)

func (cli *CWMan) CmdCreateNamespace(args ...string) error {
    cmd := cli.Subcmd("create-namespace", "NAMESPACE")
    cmd.Require(mflag.Exact, 1)
    cmd.ParseFlags(args, true)

    scm, err := scm.New()
    if err != nil {
        return err
    }
    return scm.CreateNamespace(cmd.Arg(0))
}

func (cli *CWMan) CmdRemoveNamespace(args ...string) error {
    cmd := cli.Subcmd("remove-namespace", "NAMESPACE")
    cmd.Require(mflag.Exact, 1)
    cmd.ParseFlags(args, true)

    scm, err := scm.New()
    if err != nil {
        return err
    }
    return scm.RemoveNamespace(cmd.Arg(0))
}

func (cli *CWMan) CmdCreateRepo(args ... string) error {
    cmd := cli.Subcmd("create-repo", "NAMESPACE NAME")
    cmd.Require(mflag.Exact, 2)
    cmd.ParseFlags(args, true)

    scm, err := scm.New()
    if err != nil {
        return err
    }
    return scm.CreateRepo(cmd.Arg(0), cmd.Arg(1))
}

func (cli *CWMan) CmdRemoveRepo(args ...string) error {
    cmd := cli.Subcmd("remove-repo", "NAMESPACE NAME")
    cmd.Require(mflag.Exact, 2)
    cmd.ParseFlags(args, true)

    scm, err := scm.New()
    if err != nil {
        return err
    }
    return scm.RemoveRepo(cmd.Arg(0), cmd.Arg(1))
}

func (cli *CWMan) CmdAddKey(args ...string) error {
    cmd := cli.Subcmd("add-key", "NAMESPACE KEY-FILE")
    cmd.Require(mflag.Exact, 2)
    cmd.ParseFlags(args, true)

    namespace := cmd.Arg(0)
    key, err := ioutil.ReadFile(cmd.Arg(1))
    if err != nil {
        return err
    }

    scm, err := scm.New()
    if err != nil {
        return err
    }
    return scm.AddKey(namespace, string(key))
}

func (cli *CWMan) CmdRemoveKey(args ...string) error {
    cmd := cli.Subcmd("remove-key", "NAMESPACE KEY-FILE")
    cmd.Require(mflag.Exact, 2)
    cmd.ParseFlags(args, true)

    namespace := cmd.Arg(0)
    key, err := ioutil.ReadFile(cmd.Arg(1))
    if err != nil {
        return err
    }

    scm, err := scm.New()
    if err != nil {
        return err
    }
    return scm.RemoveKey(namespace, string(key))
}

func (cli *CWMan) CmdListKeys(args ...string) error {
    cmd := cli.Subcmd("list-keys", "NAMESPACE")
    cmd.Require(mflag.Exact, 1)
    cmd.ParseFlags(args, true)

    scm, err := scm.New()
    if err != nil {
        return err
    }

    keys, err := scm.ListKeys(cmd.Arg(0))
    if err != nil {
        return err
    }

    for _, key := range keys {
        fmt.Println(key.Text)
    }
    return nil
}

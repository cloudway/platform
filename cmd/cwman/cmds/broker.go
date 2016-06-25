package cmds

import (
    "github.com/cloudway/platform/broker"
    "github.com/cloudway/platform/auth/userdb"
)

func (cli *CWMan) NewUserBroker(username string) (*broker.UserBroker, error) {
    br, err := broker.New(cli.DockerClient)
    if err != nil {
        return nil, err
    }

    var user userdb.BasicUser
    err = br.Users.Find(username, &user)
    if err != nil {
        return nil, err
    }

    return br.NewUserBroker(&user), nil
}

func (cli *CWMan) NewUserBrokerFromNamespace(namespace string) (*broker.UserBroker, error) {
    br, err := broker.New(cli.DockerClient)
    if err != nil {
        return nil, err
    }

    user, err := br.Users.FindByNamespace(namespace)
    if err != nil {
        return nil, err
    }

    return br.NewUserBroker(user), nil
}

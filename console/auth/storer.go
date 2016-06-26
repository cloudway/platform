package auth

import (
    "time"
    "gopkg.in/authboss.v0"
    "github.com/cloudway/platform/auth/userdb"
    "github.com/cloudway/platform/broker"
)

type AuthbossUser struct {
    basic *userdb.BasicUser

    // Auth
    Email          string    `bson:"-"`
    Password       string    `bson:"-"`

    // OAuth2
    Oauth2Uid      string    `bson:",omitempty"`
    Oauth2Provider string    `bson:",omitempty"`
    Oauth2Token    string    `bson:",omitempty"`
    Oauth2Refresh  string    `bson:",omitempty"`
    Oauth2Expiry   time.Time `bson:",omitempty"`

    // Confirm
    ConfirmToken   string    `bson:",omitempty"`
    Confirmed      bool      `bson:",omitempty"`

    // Lock
    AttemptNumber  int64     `bson:",omitempty"`
    AttemptTime    time.Time `bson:",omitempty"`
    Locked         time.Time `bson:",omitempty"`

    // Recover
    RecoverToken   string    `bson:",omitempty"`
    RecoverTokenExpiry time.Time `bson:",omitempty"`
}

func (u *AuthbossUser) Basic() *userdb.BasicUser {
    return u.basic
}

type storeUser struct {
    userdb.BasicUser `bson:",inline"`
    Authboss AuthbossUser
}

func (u *storeUser) toAuthboss() *AuthbossUser {
    u.Authboss.basic    = &u.BasicUser
    u.Authboss.Email    = u.Name
    u.Authboss.Password = string(u.Password)
    return &u.Authboss
}

type Storer struct {
    *broker.Broker
}

func NewStorer(br *broker.Broker) Storer {
    return Storer{br}
}

func (s Storer) Create(key string, attr authboss.Attributes) error {
    var user storeUser
    if err := attr.Bind(&user.Authboss, true); err != nil {
        return err
    }

    user.Name = key
    user.Inactive = user.Authboss.ConfirmToken != ""
    return s.CreateUser(&user, user.Authboss.Password)
}

func (s Storer) Put(key string, attr authboss.Attributes) error {
    var user AuthbossUser
    if err := attr.Bind(&user, true); err != nil {
        return err
    }

    args := userdb.Args{
        "authboss": &user,
        "inactive": user.ConfirmToken != "",
    }

    err := s.Users.Update(key, args)
    if userdb.IsUserNotFound(err) {
        err = authboss.ErrUserNotFound
    }
    return err
}

func (s Storer) Get(key string) (result interface{}, err error) {
    var user storeUser
    if err := s.Users.Find(key, &user); err != nil {
        if userdb.IsUserNotFound(err) {
            err = authboss.ErrUserNotFound
        }
        return nil, err
    }
    return user.toAuthboss(), nil
}

func (s Storer) ConfirmUser(tok string) (result interface{}, err error) {
    var user storeUser
    if err := s.Users.Search(userdb.Args{"authboss.confirmtoken": tok}, &user); err != nil {
        if userdb.IsUserNotFound(err) {
            err = authboss.ErrUserNotFound
        }
        return nil, err
    }
    return user.toAuthboss(), nil
}

func (s Storer) RecoverUser(rec string) (result interface{}, err error) {
    var user storeUser
    if err := s.Users.Search(userdb.Args{"authboss.recovertoken": rec}, &user); err != nil {
        if userdb.IsUserNotFound(err) {
            err = authboss.ErrUserNotFound
        }
        return nil, err
    }
    return user.toAuthboss(), nil
}

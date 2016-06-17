package user

import (
    "net/url"
    "golang.org/x/crypto/bcrypt"
)

// An in-memory user database used for testing.
type memdb struct {
    users map[string]User
}

func init() {
    RegisterPlugin("mem", func(_ *url.URL) (UserDBPlugin, error) {
        testpass, err := bcrypt.GenerateFromPassword([]byte("test"), bcrypt.DefaultCost)
        if err != nil {
            return nil, err
        }

        db := &memdb{make(map[string]User)}
        db.Create(&User{
            Name:       "demo",
            Password:   testpass,
            Namespace:  "demo",
        })

        return db, nil
    })
}

func (db *memdb) Create(user *User) error {
    for k, u := range db.users {
        if k == user.Name {
            return DuplicateUserError{k}
        }
        if u.Namespace == user.Namespace {
            return DuplicateNamespaceError{}
        }
    }

    db.users[user.Name] = *user
    return nil
}

func (db *memdb) Find(name string) (*User, error) {
    user, ok := db.users[name]
    if ok {
        return &user, nil
    } else {
        return nil, UserNotFoundError{name}
    }
}

func (db *memdb) Remove(name string) error {
    if _, ok := db.users[name]; ok {
        delete(db.users, name)
        return nil
    } else {
        return UserNotFoundError{name}
    }
}

func (db *memdb) Update(name string, info *User) error {
    if _, ok := db.users[name]; ok {
        db.users[name] = *info
        return nil
    } else {
        return UserNotFoundError{name}
    }
}

func (db *memdb) Close() error {
    return nil
}

package user

import (
    "net/url"
    "gopkg.in/mgo.v2"
    "gopkg.in/mgo.v2/bson"
)

// User database backed by MongoDB database.
type mongodb struct {
    *mgo.Database
    users *mgo.Collection
}

func init() {
    RegisterPlugin("mongodb", func(u *url.URL) (UserDBPlugin, error) {
        session, err := mgo.Dial(u.String())
        if err != nil {
            return nil, err
        }

        db := &mongodb{Database: session.DB("")}
        db.users = db.C("users")

        if err = ensureUniqueIndex(db.users, "name"); err != nil {
            db.Close()
            return nil, err
        }
        if err = ensureUniqueIndex(db.users, "namespace"); err != nil {
            db.Close()
            return nil, err
        }

        return db, nil
    })
}

func ensureUniqueIndex(c *mgo.Collection, key string) error {
    return c.EnsureIndex(mgo.Index{
        Key:    []string{key},
        Unique: true,
    })
}

func (db *mongodb) Create(user User) error {
    err := db.users.Insert(user)
    if mgo.IsDup(err) {
        err = DuplicateUserError{user.GetName()}
    }
    return err
}

func (db *mongodb) Find(name string, result User) error {
    err := db.users.Find(bson.M{"name": name}).One(result)
    if err == mgo.ErrNotFound {
        err = UserNotFoundError{name}
    }
    return err
}

func (db *mongodb) Search(filter interface{}, result interface{}) error {
    return db.users.Find(filter).All(result)
}

func (db *mongodb) Remove(name string) error {
    err := db.users.Remove(bson.M{"name": name})
    if err == mgo.ErrNotFound {
        err = UserNotFoundError{name}
    }
    return err
}

func (db *mongodb) Update(name string, fields interface{}) error {
    err := db.users.Update(bson.M{"name": name}, bson.M{"$set": fields})
    if err == mgo.ErrNotFound {
        err = UserNotFoundError{name}
    } else if mgo.IsDup(err) {
        err = DuplicateNamespaceError{name}
    }
    return err
}

func (db *mongodb) Close() error {
    db.Session.Close()
    return nil
}

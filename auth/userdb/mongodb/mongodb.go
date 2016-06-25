package mongodb

import (
    "fmt"
    "errors"
    "strings"
    "reflect"
    "gopkg.in/mgo.v2"
    "gopkg.in/mgo.v2/bson"
    "github.com/cloudway/platform/container/conf"
    "github.com/cloudway/platform/auth/userdb"
)

// User database backed by MongoDB database.
type mongodb struct {
    *mgo.Database
    users *mgo.Collection
}

func init() {
    prev := userdb.NewPlugin
    userdb.NewPlugin = func() (userdb.Plugin, error) {
        dbtype := conf.Get("userdb.type")
        dburl  := conf.Get("userdb.url")

        if dbtype != "" && dbtype != "mongodb" {
            return prev()
        }
        if dbtype == "" && !strings.HasPrefix(dburl, "mongodb://") {
            return prev()
        }
        if dburl == "" {
            return nil, errors.New("MongoDB URL not configured")
        }

        session, err := mgo.Dial(dburl)
        if err != nil {
            return nil, err
        }

        db := &mongodb{Database: session.DB("")}
        db.users = db.C("users")

        err = db.users.EnsureIndex(mgo.Index{
            Key:    []string{"name"},
            Unique: true,
        })
        if err != nil {
            db.Close()
            return nil, err
        }

        err = db.users.EnsureIndexKey("namespace")
        if err != nil {
            db.Close()
            return nil, err
        }

        return db, nil
    }
}

func (db *mongodb) Create(user userdb.User) error {
    basic := user.Basic()

    if basic.Namespace != "" {
        n, err := db.users.Find(bson.M{"namespace": basic.Namespace}).Count()
        if err != nil {
            return err
        }
        if n != 0 {
            return userdb.DuplicateNamespaceError(basic.Namespace)
        }
    }

    err := db.users.Insert(user)
    if mgo.IsDup(err) {
        err = userdb.DuplicateUserError(basic.Name)
    }
    return err
}

func (db *mongodb) SetNamespace(username, namespace string) error {
    var user userdb.BasicUser
    err := db.users.Find(bson.M{"namespace": namespace}).One(&user)
    if err != nil && err != mgo.ErrNotFound {
        return err
    }
    if err == nil {
        if user.Name == username {
            return nil
        } else {
            return userdb.DuplicateNamespaceError(namespace)
        }
    }
    return db.users.Update(
        bson.M{"name": username},
        bson.M{"$set": bson.M{"namespace": namespace}})
}

func (db *mongodb) Find(name string, result userdb.User) error {
    err := db.users.Find(bson.M{"name": name}).One(result)
    if err == mgo.ErrNotFound {
        err = userdb.UserNotFoundError(name)
    }
    return err
}

func (db *mongodb) Search(filter interface{}, result interface{}) error {
    resultv := reflect.ValueOf(result)
    if resultv.Kind() == reflect.Ptr && resultv.Elem().Kind() == reflect.Slice {
        return db.users.Find(filter).All(result)
    } else {
        err := db.users.Find(filter).One(result)
        if err == mgo.ErrNotFound {
            err = userdb.UserNotFoundError(fmt.Sprintf("%v", filter))
        }
        return err
    }
}

func (db *mongodb) Remove(name string) error {
    err := db.users.Remove(bson.M{"name": name})
    if err == mgo.ErrNotFound {
        err = userdb.UserNotFoundError(name)
    }
    return err
}

func (db *mongodb) Update(name string, fields interface{}) error {
    err := db.users.Update(bson.M{"name": name}, bson.M{"$set": fields})
    if err == mgo.ErrNotFound {
        err = userdb.UserNotFoundError(name)
    }
    return err
}

func (db *mongodb) Close() error {
    db.Session.Close()
    return nil
}

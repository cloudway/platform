package mongodb

import (
	"errors"
	"fmt"
	"reflect"
	"strings"

	"gopkg.in/mgo.v2"
	"gopkg.in/mgo.v2/bson"

	"github.com/cloudway/platform/auth/userdb"
	"github.com/cloudway/platform/config"
)

// User database backed by MongoDB database.
type mongodb struct {
	session *mgo.Session
}

func init() {
	prev := userdb.NewPlugin
	userdb.NewPlugin = func() (userdb.Plugin, error) {
		dbtype := config.Get("userdb.type")
		dburl := config.Get("userdb.url")

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

		users := session.DB("").C("users")

		err = users.EnsureIndex(mgo.Index{
			Key:    []string{"name"},
			Unique: true,
		})
		if err != nil {
			session.Close()
			return nil, err
		}

		err = users.EnsureIndexKey("namespace")
		if err != nil {
			session.Close()
			return nil, err
		}

		return &mongodb{session}, nil
	}
}

func (db *mongodb) acquire() *mgo.Collection {
	session := db.session.Copy()
	return session.DB("").C("users")
}

func (db *mongodb) release(c *mgo.Collection) {
	c.Database.Session.Close()
}

func (db *mongodb) Create(user userdb.User) error {
	users := db.acquire()
	defer db.release(users)

	basic := user.Basic()

	if basic.Namespace != "" {
		n, err := users.Find(bson.M{"namespace": basic.Namespace}).Count()
		if err != nil {
			return err
		}
		if n != 0 {
			return userdb.DuplicateNamespaceError(basic.Namespace)
		}
	}

	err := users.Insert(user)
	if mgo.IsDup(err) {
		err = userdb.DuplicateUserError(basic.Name)
	}
	return err
}

func (db *mongodb) SetNamespace(username, namespace string) error {
	users := db.acquire()
	defer db.release(users)

	if namespace != "" {
		var user userdb.BasicUser
		err := users.Find(bson.M{"namespace": namespace}).One(&user)
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
	}

	return users.Update(
		bson.M{"name": username},
		bson.M{"$set": bson.M{"namespace": namespace}})
}

func (db *mongodb) Find(name string, result userdb.User) error {
	users := db.acquire()
	defer db.release(users)

	err := users.Find(bson.M{"name": name}).One(result)
	if err == mgo.ErrNotFound {
		err = userdb.UserNotFoundError(name)
	}
	return err
}

func (db *mongodb) Search(filter interface{}, result interface{}) error {
	users := db.acquire()
	defer db.release(users)

	resultv := reflect.ValueOf(result)
	if resultv.Kind() == reflect.Ptr && resultv.Elem().Kind() == reflect.Slice {
		return users.Find(filter).All(result)
	} else {
		err := users.Find(filter).One(result)
		if err == mgo.ErrNotFound {
			err = userdb.UserNotFoundError(fmt.Sprintf("%v", filter))
		}
		return err
	}
}

func (db *mongodb) Remove(name string) error {
	users := db.acquire()
	defer db.release(users)

	err := users.Remove(bson.M{"name": name})
	if err == mgo.ErrNotFound {
		err = userdb.UserNotFoundError(name)
	}
	return err
}

func (db *mongodb) Update(name string, fields interface{}) error {
	users := db.acquire()
	defer db.release(users)

	err := users.Update(bson.M{"name": name}, bson.M{"$set": fields})
	if err == mgo.ErrNotFound {
		err = userdb.UserNotFoundError(name)
	}
	return err
}

func (db *mongodb) GetSecret(key string, gen func() []byte) ([]byte, error) {
	session := db.session.Copy()
	c := session.DB("").C("secret")
	defer session.Close()

	var record struct {
		Secret []byte
	}

	err := c.FindId(key).One(&record)
	if err == mgo.ErrNotFound {
		record.Secret = gen()
		err = c.Insert(bson.M{"_id": key, "secret": record.Secret})
	}
	return record.Secret, err
}

func (db *mongodb) Close() error {
	db.session.Close()
	return nil
}

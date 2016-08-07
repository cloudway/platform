package userdb

import (
    "fmt"
    "net/http"
    "net/url"
    "strings"
    "golang.org/x/crypto/bcrypt"
    "github.com/cloudway/platform/config"
)

// The Plugin interface represents a user database plugin. This interface
// provides CRUD operations for users. The user database can be backed by
// relational or NoSQL database, LDAP or Kerberos services.
type Plugin interface {
    // Create a new user in the database.
    Create(user User) error

    // Set the namespace for the given user. The namespace must be unique.
    SetNamespace(username, namespace string) error

    // Find the user by name.
    Find(name string, result User) error

    // Searchs user database by the given filter.
    Search(filter interface{}, result interface{}) error

    // Remove the user from the database.
    Remove(name string) error

    // Update user with the new data.
    Update(name string, fields interface{}) error

    // Close the user database.
    Close() error
}

var NewPlugin = func() (Plugin, error) {
    dbtype := config.Get("userdb.type")
    dburl  := config.Get("userdb.url")

    if dbtype == "" && dburl != "" {
        u, err := url.Parse(dburl)
        if err != nil {
            return nil, err
        }
        dbtype = u.Scheme
    }

    if dbtype == "" {
        return nil, fmt.Errorf("The user database plugin does not configured")
    } else {
        return nil, fmt.Errorf("Unsupported user database scheme: %s", dbtype)
    }
}

// Utility type to create filters and update fields.
type Args map[string]interface{}

// The DuplicateUserError indicates that an user already exists in the database
// when creating user.
type DuplicateUserError string

// The DuplicateNamespaceError indicates that a namespace already exists in the
// database when creating or modifying user.
type DuplicateNamespaceError string

// The UserNotFoundError indicates that a user not found in the database.
type UserNotFoundError string

// The InvalidUserError indicates that a user is not valid to login.
type InactiveUserError string

func (e DuplicateUserError) Error() string {
    return fmt.Sprintf("User already exists: %s", string(e))
}

func (e DuplicateUserError) HTTPErrorStatusCode() int {
    return http.StatusConflict
}

func (e DuplicateNamespaceError) Error() string {
    return fmt.Sprintf("Namespace already in use: %s", string(e))
}

func (e DuplicateNamespaceError) HTTPErrorStatusCode() int {
    return http.StatusConflict
}

func (e UserNotFoundError) Error() string {
    return fmt.Sprintf("User not found: %s", string(e))
}

func (e UserNotFoundError) HTTPErrorStatusCode() int {
    return http.StatusNotFound
}

func IsUserNotFound(err error) bool {
    _, ok := err.(UserNotFoundError)
    return ok
}

func (e InactiveUserError) Error() string {
    return fmt.Sprintf("You cannot login using this identity: %s", string(e))
}

func (e InactiveUserError) HTTPErrorStatusCode() int {
    return http.StatusUnauthorized
}

// The UserDatabase type is the central point of user management.
type UserDatabase struct {
    plugin Plugin
}

func Open() (*UserDatabase, error) {
    plugin, err := NewPlugin()
    if err != nil {
        return nil, err
    }
    return &UserDatabase{plugin}, nil
}

func (db *UserDatabase) Create(user User, password string) error {
    basic := user.Basic()

    if basic.Name == "" || len(password) == 0 {
        return fmt.Errorf("Missing required parameters")
    }

    hashedPassword, err := hashPassword(password)
    if err != nil {
        return err
    }

    basic.Password = hashedPassword
    return db.plugin.Create(user)
}

func hashPassword(password string) ([]byte, error) {
    // use the password if it's already hashed
    if strings.HasPrefix(password, "$2a$") {
        if _, err := bcrypt.Cost([]byte(password)); err == nil {
            return []byte(password), nil
        }
    }

    // otherwise, generate a hashed password
    return bcrypt.GenerateFromPassword([]byte(password), bcrypt.DefaultCost)
}

func (db *UserDatabase) SetNamespace(username, namespace string) error {
    return db.plugin.SetNamespace(username, namespace)
}

func (db *UserDatabase) Find(name string, result User) error {
    return db.plugin.Find(name, result)
}

func (db *UserDatabase) FindByNamespace(namespace string) (User, error) {
    var user BasicUser
    err := db.Search(Args{"namespace": namespace}, &user)
    return &user, err
}

func (db *UserDatabase) Search(filter interface{}, result interface{}) error {
    return db.plugin.Search(filter, result)
}

func (db *UserDatabase) Remove(name string) error {
    return db.plugin.Remove(name)
}

func (db *UserDatabase) Update(name string, fields interface{}) error {
    return db.plugin.Update(name, fields)
}

func (db *UserDatabase) Authenticate(name string, password string) (*BasicUser, error) {
    var user BasicUser
    if err := db.plugin.Find(name, &user); err != nil {
        return nil, err
    }

    if user.Inactive {
        return nil, InactiveUserError(name)
    }

    err := bcrypt.CompareHashAndPassword(user.Password, []byte(password))
    if err != nil {
        return nil, err
    }

    return &user, nil
}

func (db *UserDatabase) ChangePassword(name string, oldPassword, newPassword string) error {
    var user BasicUser
    if err := db.plugin.Find(name, &user); err != nil {
        return err
    }

    err := bcrypt.CompareHashAndPassword(user.Password, []byte(oldPassword))
    if err != nil {
        return err
    }

    hashedPassword, err := hashPassword(newPassword)
    if err != nil {
        return err
    }

    return db.plugin.Update(name, Args{"password": hashedPassword})
}

func (db *UserDatabase) Close() error {
    return db.plugin.Close()
}

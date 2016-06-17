package user

import (
    "fmt"
    "net/http"
    "net/url"
    "errors"
    "golang.org/x/crypto/bcrypt"
    "github.com/cloudway/platform/container/conf"
)

// The UserDBPlugin interface represents a user database plugin.
// The user database can be backed by relational or NoSQL database,
// LDAP or Kerberos services.
type UserDBPlugin interface {
    // Create a new user in the database.
    Create(user *User) error

    // Find the user by name.
    Find(name string) (*User, error)

    // Remove the user from the database.
    Remove(name string) error

    // Update user with the new information.
    Update(name string, user *User) error

    // Close the user database.
    Close() error
}

// The DuplicateUserError indicates that an user already exists in the database
// when creating user.
type DuplicateUserError struct {
    Name string
}

// The DuplicateNamespaceError indicates that a namespace already exists in the
// database when creating or modifying user.
type DuplicateNamespaceError struct {
    Namespace string
}

// The UserNotFoundError indicates that a user not found in the database.
type UserNotFoundError struct {
    Name string
}

func (e DuplicateUserError) Error() string {
    return fmt.Sprintf("User already exists: %s", e.Name)
}

func (e DuplicateUserError) HTTPErrorStatusCode() int {
    return http.StatusConflict
}

func (e DuplicateNamespaceError) Error() string {
    return fmt.Sprintf("Namespace already exists: %s", e.Namespace)
}

func (e DuplicateNamespaceError) HTTPErrorStatusCode() int {
    return http.StatusConflict
}

func (e UserNotFoundError) Error() string {
    return fmt.Sprintf("User not found: %s", e.Name)
}

func (e UserNotFoundError) HTTPErrorStatusCode() int {
    return http.StatusNotFound
}

// Registry for the user database plugins.
var pluginRegistry = make(map[string]func(*url.URL)(UserDBPlugin, error))

// Register a user database plugin.
func RegisterPlugin(scheme string, fn func(*url.URL)(UserDBPlugin, error)) {
    pluginRegistry[scheme] = fn
}

// The UserDatabase type is the central point of user management.
type UserDatabase struct {
    plugin UserDBPlugin
}

// Open the user database by the specified URL.
func OpenUserDatabase() (*UserDatabase, error) {
    dbUrl := conf.Get("userdb-url")
    if dbUrl == "" {
        return nil, errors.New("User database URL not configured")
    }

    u, err := url.Parse(dbUrl)
    if err != nil {
        return nil, err
    }

    fn := pluginRegistry[u.Scheme]
    if fn == nil {
        return nil, fmt.Errorf("Unsupported user database scheme: %s", u.Scheme)
    }

    plugin, err := fn(u)
    if err != nil {
        return nil, err
    }

    return &UserDatabase{plugin}, nil
}

func (db *UserDatabase) Create(user *User) error {
    if user.Name == "" || user.Namespace == "" || user.Password == nil {
        return fmt.Errorf("Missing required parameters")
    }

    hashedPassword, err := bcrypt.GenerateFromPassword(user.Password, bcrypt.DefaultCost)
    if err != nil {
        return err
    }

    user.HashedPassword = hashedPassword
    return db.plugin.Create(user)
}

func (db *UserDatabase) Find(name string) (*User, error) {
    user, err := db.plugin.Find(name)
    if err != nil {
        return nil, err
    }
    return user, nil
}

func (db *UserDatabase) Authenticate(name string, password []byte) (*User, error) {
    user, err := db.plugin.Find(name)
    if err != nil {
        return nil, err
    }

    err = bcrypt.CompareHashAndPassword(user.HashedPassword, password)
    if err != nil {
        return nil, err
    }

    return user, nil
}

func (db *UserDatabase) Remove(name string) error {
    return db.plugin.Remove(name)
}

func (db *UserDatabase) ChangePassword(name string, oldPassword, newPassword []byte) error {
    user, err := db.plugin.Find(name)
    if err != nil {
        return err
    }

    err = bcrypt.CompareHashAndPassword(user.HashedPassword, oldPassword)
    if err != nil {
        return err
    }

    hashedPassword, err := bcrypt.GenerateFromPassword(newPassword, bcrypt.DefaultCost)
    if err != nil {
        return err
    }

    user.HashedPassword = hashedPassword
    return db.plugin.Update(name, user)
}

func (db *UserDatabase) Close() error {
    return db.plugin.Close()
}

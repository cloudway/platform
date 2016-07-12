package userdb

import "time"

// The User interface encapsulates a cloud user. The concret User type must
// embedded a BasicUser struct that contains core information that used by
// cloudway controller. Extra fields may be maintained by concret User type
// and these fields will be written to the user database.
type User interface {
    // Basic returns the core information of a User.
    Basic() *BasicUser
}

// The basic User interface implementation.
type BasicUser struct {
    Name         string
    Namespace    string
    Password     []byte
    Inactive     bool
    Applications map[string]*Application
}

type Application struct {
    CreatedAt   time.Time
    Plugins     []string
    Hosts       []string `bson:",omitempty"`
    Secret      string
}

func (user *BasicUser) Basic() *BasicUser {
    return user
}

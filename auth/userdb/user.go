package userdb

type User interface {
    // Returns the user name.
    GetName() string

    // Returns the application namespace associated to the user.
    GetNamespace() string

    // Returns the hashed password that stored in the user database.
    GetPassword() []byte

    // Set the hashed password.
    SetPassword(password []byte)
}

// The basic User interface implementation.
type BasicUser struct {
    Name      string
    Namespace string
    Password  []byte
}

func (user *BasicUser) GetName() string {
    return user.Name
}

func (user *BasicUser) GetNamespace() string {
    return user.Namespace
}

func (user *BasicUser) GetPassword() []byte {
    return user.Password
}

func (user *BasicUser) SetPassword(password []byte) {
    cp := make([]byte, len(password))
    copy(cp, password)
    user.Password = cp
}

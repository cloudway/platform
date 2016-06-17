package user

type User struct {
    // The user name.
    Name string `json:"user"`

    // The application namespace associated to the user.
    Namespace string `json:"ns"`

    // The plain text user password. This is only used to create a user.
    Password []byte `json:"-" bson:"-"`

    // Hashed password that stored in the uesr database.
    HashedPassword []byte `json:"-" bson:"password"`
}

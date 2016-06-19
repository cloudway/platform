package scm

import (
    "fmt"
    "net/http"
)

type NamespaceNotFoundError string
type NamespaceExistError string
type RepoNotFoundError string
type RepoExistError string
type InvalidKeyError struct{}

func (e NamespaceNotFoundError) Error() string {
    return fmt.Sprintf("The namespace '%s' does not exists", string(e))
}

func (e NamespaceNotFoundError) HTTPErrorStatusCode() int {
    return http.StatusNotFound
}

func (e NamespaceExistError) Error() string {
    return fmt.Sprintf("The namespace '%s' is already exists", string(e))
}

func (e NamespaceExistError) HTTPErrorStatusCode() int {
    return http.StatusConflict
}

func (e RepoNotFoundError) Error() string {
    return fmt.Sprintf("The repository '%s' does not exists", string(e))
}

func (e RepoNotFoundError) HTTPStatusCode() int {
    return http.StatusNotFound
}

func (e RepoExistError) Error() string {
    return fmt.Sprintf("The repository '%s' is already exists", string(e))
}

func (e RepoExistError) HTTPStatusCode() int {
    return http.StatusConflict
}

func (e InvalidKeyError) Error() string {
    return "You must enter a valid SSH public key"
}

func (e InvalidKeyError) HTTPStatusCode() int {
    return http.StatusBadRequest
}

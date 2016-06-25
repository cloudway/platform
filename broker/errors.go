package broker

import (
    "fmt"
    "net/http"
)

type ApplicationNotFoundError string

func (e ApplicationNotFoundError) Error() string {
    return fmt.Sprintf("Application '%s' not found", string(e))
}

func (e ApplicationNotFoundError) HTTPErrorStatusCode() int {
    return http.StatusNotFound
}

type ApplicationExistError struct {
    Name, Namespace string
}

func (e ApplicationExistError) Error() string {
    return fmt.Sprintf("The application '%s' already exists in the namespace '%s'", e.Name, e.Namespace)
}

func (e ApplicationExistError) HTTPErrorStatusCode() int {
    return http.StatusConflict
}

type NoNamespaceError string

func (e NoNamespaceError) Error() string {
    return fmt.Sprintf("No namespace created for the user '%s'", string(e))
}

func (e NoNamespaceError) HTTPErrorStatusCode() int {
    return http.StatusBadRequest
}

type NamespaceNotEmptyError string

func (e NamespaceNotEmptyError) Error() string {
    return fmt.Sprintf("Cannot remove namespace '%s' because it contains one or more applications", string(e))
}

func (e NamespaceNotEmptyError) HTTPErrorStatusCode() int {
    return http.StatusForbidden
}


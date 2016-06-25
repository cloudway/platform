package errors

import (
    "fmt"
    "bytes"
)

// A composite error type.
type Errors struct {
    errors []error
}

func (e Errors) Error() string {
    var buf bytes.Buffer
    for _, err := range e.errors {
        fmt.Fprintln(&buf, err.Error())
    }
    return buf.String()
}

func (e Errors) Add(err error) {
    if err != nil {
        e.errors = append(e.errors, err)
    }
}

func (e Errors) Err() error {
    if len(e.errors) != 0 {
        return e
    } else {
        return nil
    }
}

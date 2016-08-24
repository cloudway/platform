package opts

import (
	"fmt"
	"os"
	"strings"
)

// ListOpts holds a list of values and a validation function
type ListOpts struct {
	values    *[]string
	validator ValidatorFunc
}

// NewListOpts creates a new ListOpts with the specified validator.
func NewListOpts(validator ValidatorFunc) ListOpts {
	var values []string
	return *NewListOptsRef(&values, validator)
}

// NewListOptsRef creates a new ListOpts with the specified values and validator.
func NewListOptsRef(values *[]string, validator ValidatorFunc) *ListOpts {
	return &ListOpts{
		values:    values,
		validator: validator,
	}
}

func (opts *ListOpts) String() string {
	return fmt.Sprintf("%v", *opts.values)
}

// Set validates if needed the input value and adds it to the internal slice.
func (opts *ListOpts) Set(value string) error {
	if opts.validator != nil {
		v, err := opts.validator(value)
		if err != nil {
			return err
		}
		value = v
	}
	*opts.values = append(*opts.values, value)
	return nil
}

// GetAll returns the values of slice.
func (opts *ListOpts) GetAll() []string {
	return *opts.values
}

// MapOpts holds a map of values and validation function
type MapOpts struct {
	values    *map[string]string
	validator ValidatorFunc
}

// NewMapOpts creates a new MapOpts with the specified map of values and a validator.
func NewMapOpts(validator ValidatorFunc) MapOpts {
	values := make(map[string]string)
	return *NewMapOptsRef(&values, validator)
}

// NewMapOptsRef creates a new MapOpts with the specified values and validator.
func NewMapOptsRef(values *map[string]string, validator ValidatorFunc) *MapOpts {
	if *values == nil {
		*values = make(map[string]string)
	}
	return &MapOpts{
		values:    values,
		validator: validator,
	}
}

func (opts *MapOpts) String() string {
	return fmt.Sprintf("%v", *opts.values)
}

// Set validates if needed the input value and add it to the
// internal map, by splitting on '='.
func (opts *MapOpts) Set(value string) error {
	if opts.validator != nil {
		v, err := opts.validator(value)
		if err != nil {
			return err
		}
		value = v
	}
	vals := strings.SplitN(value, "=", 2)
	if len(vals) == 1 {
		(*opts.values)[vals[0]] = ""
	} else {
		(*opts.values)[vals[0]] = vals[1]
	}
	return nil
}

// GetAll returns the values of MapOpts as a map.
func (opts *MapOpts) GetAll() map[string]string {
	return *opts.values
}

// ValidatorFunc defines a validator function that returns a validated string and/or an error.
type ValidatorFunc func(val string) (string, error)

// ValidateEnv validates an environment variable and returns it.
// If no value is specified, it returns the current value using os.Getenv.
func ValidateEnv(val string) (string, error) {
	if strings.Contains(val, "=") {
		return val, nil
	}
	if value, ok := os.LookupEnv(val); ok {
		return fmt.Sprintf("%s=%s", val, value), nil
	}
	return val, nil
}

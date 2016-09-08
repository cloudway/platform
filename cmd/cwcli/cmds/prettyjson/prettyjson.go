// Package prettyjson provides JSON pretty print
package prettyjson

import (
	"encoding/json"
	"fmt"
	"sort"
	"strconv"
	"strings"

	"github.com/cloudway/platform/cmd/cwcli/cmds/ansi"
)

// Formatter is a struct to format JSON data.
type Formatter struct {
	// JSON key color.
	KeyColor *ansi.Color

	// JSON string value color.
	StringColor *ansi.Color

	// JSON boolean value color.
	BoolColor *ansi.Color

	// JSON number value color.
	NumberColor *ansi.Color

	// JSON null value color.
	NullColor *ansi.Color

	// Boolean to disable color. Default is false.
	DisableColor bool

	// Indent space number. Default is 2.
	Indent int
}

// NewFormatter returns a new formatter with following default values.
func NewFormatter() *Formatter {
	return &Formatter{
		KeyColor:     ansi.NewColor(ansi.FgBlue, ansi.Bold),
		StringColor:  nil,
		BoolColor:    ansi.NewColor(ansi.FgYellow, ansi.Bold),
		NumberColor:  ansi.NewColor(ansi.FgMagenta),
		NullColor:    ansi.NewColor(ansi.Bold),
		DisableColor: false,
		Indent:       2,
	}
}

// Marshals and formats JSON data.
func (f *Formatter) Marshal(v interface{}) ([]byte, error) {
	data, err := json.Marshal(v)
	if err != nil {
		return nil, err
	}
	return f.Format(data)
}

// Formats JSON string.
func (f *Formatter) Format(data []byte) ([]byte, error) {
	var v interface{}
	err := json.Unmarshal(data, &v)
	if err != nil {
		return nil, err
	}

	s := f.pretty(v, 1)
	return []byte(s), nil
}

func (f *Formatter) colorize(c *ansi.Color, val string) string {
	if f.DisableColor || c == nil {
		return val
	} else {
		return c.Wrap(val)
	}
}

func (f *Formatter) pretty(v interface{}, depth int) string {
	switch val := v.(type) {
	case string:
		return f.processString(val)
	case float64:
		return f.colorize(f.NumberColor, strconv.FormatFloat(val, 'f', -1, 64))
	case bool:
		return f.colorize(f.BoolColor, strconv.FormatBool(val))
	case nil:
		return f.colorize(f.NullColor, "null")
	case map[string]interface{}:
		return f.processMap(val, depth)
	case []interface{}:
		return f.processArray(val, depth)
	}

	return ""
}

func (f *Formatter) processString(s string) string {
	b, _ := json.Marshal(s)
	return f.colorize(f.StringColor, string(b))
}

func (f *Formatter) processMap(m map[string]interface{}, depth int) string {
	currentIndent := f.generateIndent(depth - 1)
	nextIndent := f.generateIndent(depth)
	rows := []string{}
	keys := []string{}

	if len(m) == 0 {
		return "{}"
	}

	for key := range m {
		keys = append(keys, key)
	}

	sort.Strings(keys)

	for _, key := range keys {
		val := m[key]
		k := f.colorize(f.KeyColor, fmt.Sprintf("%q", key))
		v := f.pretty(val, depth+1)
		row := fmt.Sprintf("%s%s: %s", nextIndent, k, v)
		rows = append(rows, row)
	}

	return fmt.Sprintf("{\n%s\n%s}", strings.Join(rows, ",\n"), currentIndent)
}

func (f *Formatter) processArray(a []interface{}, depth int) string {
	currentIndent := f.generateIndent(depth - 1)
	nextIndent := f.generateIndent(depth)
	rows := []string{}

	if len(a) == 0 {
		return "[]"
	}

	if len(a) == 1 {
		c := f.pretty(a[0], depth+1)
		if !strings.ContainsRune(c, '\n') {
			return fmt.Sprintf("[%s]", c)
		}
	}

	for _, val := range a {
		c := f.pretty(val, depth+1)
		row := nextIndent + c
		rows = append(rows, row)
	}

	return fmt.Sprintf("[\n%s\n%s]", strings.Join(rows, ",\n"), currentIndent)
}

func (f *Formatter) generateIndent(depth int) string {
	return strings.Repeat(" ", f.Indent*depth+1)
}

// Marshal JSON data with default options.
func Marshal(v interface{}) ([]byte, error) {
	return NewFormatter().Marshal(v)
}

// Format JSON string with default options.
func Format(data []byte) ([]byte, error) {
	return NewFormatter().Format(data)
}

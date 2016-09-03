package ansi

import (
	"fmt"
	"os"
	"strconv"
	"strings"

	"golang.org/x/crypto/ssh/terminal"
)

var IsTerminal = terminal.IsTerminal(int(os.Stdout.Fd()))

// Color defines a custom color object which is defined by SGR parameters.
type Color struct {
	params []Attribute
}

// Attribute defineds a single SGR code.
type Attribute int

// Base attributes
const (
	Reset Attribute = iota
	Bold
	Faint
	Italic
	Underline
	BlinkSlow
	BlinkRapid
	Reverse
	Concealed
	CrossedOut
)

// Foreground text colors
const (
	FgBlack Attribute = iota + 30
	FgRed
	FgGreen
	FgYellow
	FgBlue
	FgMagenta
	FgCyan
	FgWhite
)

// Foreground Hi-Intensity text colors
const (
	FgHiBlack Attribute = iota + 90
	FgHiRed
	FgHiGreen
	FgHiYellow
	FgHiBlue
	FgHiMagenta
	FgHiCyan
	FgHiWhite
)

// Background text colors
const (
	BgBlack Attribute = iota + 40
	BgRed
	BgGreen
	BgYellow
	BgBlue
	BgMagenta
	BgCyan
	ByWhite
)

// Background Hi-Intensity text colors
const (
	BgHiBlack Attribute = iota + 100
	BgHiRed
	BgHiGreen
	BgHiYellow
	BgHiBlue
	BgHiMagenta
	BgHiCyan
	BgHiWhite
)

// New returns a newly created color object.
func NewColor(value ...Attribute) *Color {
	c := &Color{params: make([]Attribute, 0)}
	c.Add(value...)
	return c
}

// Add is used to chain SGR parameters. Use as many as parameters to combine
// and create custom color objects. Example: Add(color.FgRed, color.Underline).
func (c *Color) Add(value ...Attribute) *Color {
	c.params = append(c.params, value...)
	return c
}

// wrap wraps the s string with the colors attributes. The string is ready to
// be printed
func (c *Color) Wrap(s string) string {
	if IsTerminal {
		return c.escape() + s + c.unescape()
	} else {
		return s
	}
}

// sequence returns a formatted SGR sequence to be plugged into a "\x1b[...m"
// an example output might be: "1;36" -> bold cyan
func (c *Color) sequence() string {
	format := make([]string, len(c.params))
	for i, v := range c.params {
		format[i] = strconv.Itoa(int(v))
	}
	return strings.Join(format, ";")
}

func (c *Color) escape() string {
	return fmt.Sprintf("\033[%sm", c.sequence())
}

func (c *Color) unescape() string {
	return "\033[0m"
}

// -------------------------------------------------------------------

var (
	primary = NewColor(FgBlue)
	success = NewColor(FgHiGreen)
	info    = NewColor(FgHiCyan)
	warning = NewColor(FgYellow)
	danger  = NewColor(FgHiRed)
	fail    = NewColor(FgRed)
	hilite  = NewColor(Bold)
)

func Primary(s string) string {
	return primary.Wrap(s)
}

func Success(s string) string {
	return success.Wrap(s)
}

func Info(s string) string {
	return info.Wrap(s)
}

func Warning(s string) string {
	return warning.Wrap(s)
}

func Danger(s string) string {
	return danger.Wrap(s)
}

func Fail(s string) string {
	return fail.Wrap(s)
}

func Hilite(s string) string {
	return hilite.Wrap(s)
}

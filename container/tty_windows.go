// +build windows

package container

import "time"

func (t *Tty) monitorResize(fn func (width, height int) error) {
    t.resize(fn)

    go func() {
        prevW, prevH, _ := t.getSize()
        for {
            time.Sleep(time.Millisecond * 250)
            w, h, _ := t.getSize()
            if prevW != w || prevH != h {
                t.resize(fn)
            }
            prevW, prevH = w, h
        }
    }()
}

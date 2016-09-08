// +build windows

package reap

type Reaper struct{}

func New() *Reaper {
	return &Reaper{}
}

func (r *Reaper) Run(c chan<- Child) {}

func (r *Reaper) Close()  {}
func (r *Reaper) Lock()   {}
func (r *Reaper) Unlock() {}

package broker

import (
	"context"
	"encoding/json"
	"fmt"
	"io"
	"net/http"
	"strings"
	"sync"
	"time"

	dockertypes "github.com/docker/engine-api/types"

	"github.com/cloudway/platform/api/types"
	"github.com/cloudway/platform/container"
	"github.com/cloudway/platform/hub"
)

type containerStats struct {
	c container.Container
	types.ContainerStats
	mu  sync.RWMutex
	err error
}

type stats struct {
	mu sync.Mutex
	cs []*containerStats
}

func (br *UserBroker) newContainerStats(c container.Container) *containerStats {
	s := &containerStats{}
	s.c = c
	s.ID = c.ID()
	if c.ServiceName() != "" {
		s.Name = c.ServiceName()
	} else {
		_, _, pn, _, _ := hub.ParseTag(c.PluginTag())
		s.Name = pn
	}
	return s
}

func (s *containerStats) Collect(ctx context.Context, stopChan chan struct{}, stream bool) {
	resp, err := s.c.Stats(ctx, stream)
	if err != nil {
		s.mu.Lock()
		s.err = err
		s.mu.Unlock()
		return
	}
	defer resp.Close()

	var (
		previousCPU    uint64
		previousSystem uint64
		dec            = json.NewDecoder(resp)
		u              = make(chan error, 1)
	)

	go func() {
		for {
			var v *dockertypes.StatsJSON
			if err := dec.Decode(&v); err != nil {
				u <- err
				return
			}

			var memPercent = 0.0
			var cpuPercent = 0.0

			// MemoryStats.Limit will never be 0 unless the container is not
			// running and we haven't got any data from cgroup
			if v.MemoryStats.Limit != 0 {
				memPercent = float64(v.MemoryStats.Usage) / float64(v.MemoryStats.Limit) * 100.0
			}

			previousCPU = v.PreCPUStats.CPUUsage.TotalUsage
			previousSystem = v.PreCPUStats.SystemUsage
			cpuPercent = calculateCPUPercent(previousCPU, previousSystem, v)
			blkRead, blkWrite := calculateBlockIO(&v.BlkioStats)
			netRx, netTx := calculateNetwork(v.Networks)

			s.mu.Lock()
			s.CPUTotalUsage = v.CPUStats.CPUUsage.TotalUsage
			s.CPUSystemUsage = v.CPUStats.SystemUsage
			s.CPUPercentage = cpuPercent
			s.MemoryUsage = v.MemoryStats.Usage
			s.MemoryLimit = v.MemoryStats.Limit
			s.MemoryPercentage = memPercent
			s.NetworkRx, s.NetworkTx = netRx, netTx
			s.BlockRead, s.BlockWrite = blkRead, blkWrite
			s.mu.Unlock()

			u <- nil
			if !stream {
				return
			}
		}
	}()

	for {
		select {
		case <-time.After(2 * time.Second):
			// zero out the value if we have not received an update within
			// the specified duration.
			s.mu.Lock()
			s.CPUTotalUsage = 0
			s.CPUSystemUsage = 0
			s.CPUPercentage = 0
			s.MemoryUsage = 0
			s.MemoryPercentage = 0
			s.MemoryLimit = 0
			s.NetworkRx = 0
			s.NetworkTx = 0
			s.BlockRead = 0
			s.BlockWrite = 0
			s.mu.Unlock()

		case err := <-u:
			if err != nil {
				s.mu.Lock()
				s.err = err
				s.mu.Unlock()
				return
			}

		case <-stopChan:
			return
		}

		if !stream {
			return
		}
	}
}

func (s *containerStats) Sample() (*types.ContainerStats, error) {
	s.mu.RLock()
	defer s.mu.RUnlock()

	if s.err != nil {
		return nil, s.err
	} else {
		sample := s.ContainerStats
		return &sample, nil
	}
}

func calculateCPUPercent(previousCPU, previousSystem uint64, v *dockertypes.StatsJSON) float64 {
	var (
		// calculate the change for the cpu usage of the container in between readings
		cpuDelta = float64(v.CPUStats.CPUUsage.TotalUsage) - float64(previousCPU)
		// calculate the change for the entire system between readings
		systemDelta = float64(v.CPUStats.SystemUsage) - float64(previousSystem)
	)

	if systemDelta > 0.0 && cpuDelta > 0.0 {
		return (cpuDelta / systemDelta) * float64(len(v.CPUStats.CPUUsage.PercpuUsage)) * 100.0
	} else {
		return 0.0
	}
}

func calculateBlockIO(blkio *dockertypes.BlkioStats) (blkRead, blkWrite uint64) {
	for _, bioEntry := range blkio.IoServiceBytesRecursive {
		switch strings.ToLower(bioEntry.Op) {
		case "read":
			blkRead += bioEntry.Value
		case "write":
			blkWrite += bioEntry.Value
		}
	}
	return
}

func calculateNetwork(network map[string]dockertypes.NetworkStats) (rx, tx uint64) {
	for _, v := range network {
		rx += v.RxBytes
		tx += v.TxBytes
	}
	return
}

func (br *UserBroker) Stats(name string, w io.Writer) error {
	if err := br.Refresh(); err != nil {
		return err
	}

	cs, err := br.FindAll(br.ctx, name, br.Namespace())
	if err != nil {
		return err
	}
	if len(cs) == 0 {
		return ApplicationNotFoundError(name)
	}

	var stopChan = make(chan struct{}, 1)
	defer close(stopChan)

	var cStats = stats{}
	for _, c := range cs {
		s := br.newContainerStats(c)
		cStats.cs = append(cStats.cs, s)
		go s.Collect(br.ctx, stopChan, true)
	}

	var errs []string
	cStats.mu.Lock()
	for _, s := range cStats.cs {
		s.mu.RLock()
		if s.err != nil {
			errs = append(errs, fmt.Sprintf("%s: %v", s.Name, s.err))
		}
		s.mu.RUnlock()
	}
	cStats.mu.Unlock()
	if len(errs) > 0 {
		return fmt.Errorf("%s", strings.Join(errs, ", "))
	}

	var enc = json.NewEncoder(w)
	for range time.Tick(500 * time.Millisecond) {
		var (
			toRemove = []int{}
			data     = []*types.ContainerStats{}
		)

		cStats.mu.Lock()
		for i, s := range cStats.cs {
			if sample, err := s.Sample(); err != nil {
				toRemove = append(toRemove, i)
			} else {
				data = append(data, sample)
			}
		}
		for j := len(toRemove) - 1; j >= 0; j-- {
			i := toRemove[j]
			cStats.cs = append(cStats.cs[:i], cStats.cs[i+1:]...)
		}
		cStats.mu.Unlock()

		if len(data) == 0 {
			return nil
		}
		if err = enc.Encode(data); err != nil {
			return nil
		}
		if f, ok := w.(http.Flusher); ok {
			f.Flush()
		}
	}

	return nil
}

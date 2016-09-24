package container

import (
	"context"
	"strings"
)

const EXTRA_HOSTS_KEY = "CLOUDWAY_EXTRA_HOSTS"

func (c *Container) AddHost(ctx context.Context, host string, more ...string) error {
	hosts := c.GetHosts(ctx)
	hosts = addToSlice(hosts, host)
	for _, v := range more {
		hosts = addToSlice(hosts, v)
	}
	return c.Setenv(ctx, EXTRA_HOSTS_KEY, strings.Join(hosts, ","))
}

func (c *Container) RemoveHost(ctx context.Context, host string, more ...string) error {
	hosts := c.GetHosts(ctx)
	if len(hosts) == 0 {
		return nil
	}
	hosts = removeFromSlice(hosts, host)
	for _, v := range more {
		hosts = removeFromSlice(hosts, v)
	}
	return c.Setenv(ctx, EXTRA_HOSTS_KEY, strings.Join(hosts, ","))
}

func (c *Container) GetHosts(ctx context.Context) []string {
	hosts, _ := c.Getenv(ctx, EXTRA_HOSTS_KEY)
	if hosts == "" {
		return nil
	}
	return strings.Split(hosts, ",")
}

func addToSlice(xs []string, x string) []string {
	for _, v := range xs {
		if v == x {
			return xs
		}
	}
	return append(xs, x)
}

func removeFromSlice(xs []string, x string) []string {
	for i, v := range xs {
		if v == x {
			return append(xs[:i], xs[i+1:]...)
		}
	}
	return xs
}

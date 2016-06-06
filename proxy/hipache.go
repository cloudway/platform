package proxy

import (
    "net/url"
    "github.com/cloudway/platform/plugin"
    "github.com/garyburd/redigo/redis"
    "github.com/Sirupsen/logrus"
    "strings"
)

type hipacheProxy struct {
    conn redis.Conn
}

func init() {
    proxyRegistry["hipache"] = func(u *url.URL) (Proxy, error) {
        r, err := redis.Dial("tcp", u.Host)
        if err != nil {
            return nil, err
        }
        return &hipacheProxy{conn: r}, nil
    }
}

func (px *hipacheProxy) Close() error {
    return px.conn.Close()
}

func (px *hipacheProxy) AddEndpoints(endpoints []*plugin.Endpoint) error {
    return px.forEndpoints(endpoints, addEndpoint)
}

func (px *hipacheProxy) RemoveEndpoints(endpoints []*plugin.Endpoint) error {
    return px.forEndpoints(endpoints, removeEndpoint)
}

type hipacheProxyFunc func(redis.Conn, string, string) error

func (px *hipacheProxy) forEndpoints(endpoints []*plugin.Endpoint, fn hipacheProxyFunc) error {
    for _, ep := range endpoints {
        for _, m := range ep.ProxyMappings {
            if m.Protocol == "http" && !strings.ContainsRune(m.Frontend, '/') { // FIXME
                if err := fn(px.conn, m.Frontend, m.Backend); err != nil {
                    return err
                }
            }
        }
    }
    return nil
}

func addEndpoint(conn redis.Conn, frontend, backend string) error {
    key := prefixKey(frontend)

    exists, err := redis.Bool(conn.Do("EXISTS", key))
    if err != nil {
        return err
    }

    if !exists {
        _, err = conn.Do("RPUSH", key, frontend)
        if err != nil {
            return err
        }
        logrus.Infof("add %s", frontend)
    }

    _, err = conn.Do("RPUSH", key, backend)
    if err == nil {
        logrus.Infof("add %s -> %s", frontend, backend)
    }
    return err
}

func removeEndpoint(conn redis.Conn, frontend, backend string) error {
    key := prefixKey(frontend)

    _, err := conn.Do("LREM", key, 0, backend)
    if err == nil {
        logrus.Infof("remove %s -> %s", frontend, backend)
    } else {
        return err
    }

    n, err := redis.Int(conn.Do("LLEN", key))
    if err == nil && n <= 1 {
        // remove the whole frontend
        _, err := conn.Do("DEL", key)
        if err == nil {
            logrus.Infof("remove %s", frontend)
        }
    }

    return err
}

func prefixKey(h string) string {
    return "frontend:" + h
}

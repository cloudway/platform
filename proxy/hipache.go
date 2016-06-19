package proxy

import (
    "net/url"
    "strings"
    "github.com/cloudway/platform/pkg/manifest"
    "github.com/garyburd/redigo/redis"
    "github.com/Sirupsen/logrus"
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

func (px *hipacheProxy) AddEndpoints(id string, endpoints []*manifest.Endpoint) error {
    // remove previously configured endpoints
    if err := px.RemoveEndpoints(id); err != nil {
        return err
    }

    // add new endpoints
    for _, ep := range endpoints {
        for _, m := range ep.ProxyMappings {
            if m.Protocol == "http" && !strings.ContainsRune(m.Frontend, '/') { // FIXME
                if err := addEndpoint(px.conn, id, m.Frontend, m.Backend); err != nil {
                    return err
                }
            }
        }
    }

    return nil
}

func addEndpoint(conn redis.Conn, id, frontend, backend string) error {
    key  := "frontend:" + frontend
    ckey := "container:" + id

    exists, err := redis.Bool(conn.Do("EXISTS", key))
    if err != nil {
        return err
    }

    if !exists {
        _, err = conn.Do("RPUSH", key, frontend)
        if err != nil {
            return err
        }
        logrus.Debugf("add %s", frontend)
    }

    // add endpoint record
    _, err = conn.Do("RPUSH", key, backend)
    if err != nil {
        return err
    } else {
        logrus.Debugf("add %s -> %s", key, backend)
    }

    // add container record
    _, err = conn.Do("RPUSH", ckey, key+" "+backend)
    if err != nil {
        return err
    } else {
        logrus.Debugf("add %s", ckey)
    }

    return nil
}

func (px *hipacheProxy) RemoveEndpoints(id string) error {
    key := "container:" + id

    // query endpoints by container id
    r, err := redis.Values(px.conn.Do("LRANGE", key, 0, -1))
    if err != nil {
        return err
    }

    var vs []string
    if err = redis.ScanSlice(r, &vs); err != nil {
        return err
    }
    if len(vs) == 0 {
        return nil
    }

    // remove endpoints associated to the given container
    for _, rec := range vs {
        kv := strings.SplitN(rec, " ", 2)
        frontend, backend := kv[0], kv[1]
        if _, err = px.conn.Do("LREM", frontend, 0, backend); err == nil {
            logrus.Debugf("remove %s -> %s", frontend, backend)
        }

        n, err := redis.Int(px.conn.Do("LLEN", frontend))
        if err == nil && n <= 1 {
            // remove the whole frontend
            _, err := px.conn.Do("DEL", frontend)
            if err == nil {
                logrus.Debugf("remove %s", frontend)
            }
        }
    }

    // remove container record
    if _, err = px.conn.Do("DEL", key); err == nil {
        logrus.Debugf("remove %s", key)
    }

    return err
}

func (px *hipacheProxy) Reset() error {
    // remove all keys from redis database
    _, err := px.conn.Do("FLUSHALL")
    return err
}

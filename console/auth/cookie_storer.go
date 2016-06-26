package auth

import (
    "time"
    "net/http"
    "encoding/base64"
    "gopkg.in/authboss.v0"
    "github.com/gorilla/securecookie"
)

var cookieStore *securecookie.SecureCookie

func init() {
    cookieStoreKey, _ := base64.StdEncoding.DecodeString(`NpEPi8pEjKVjLGJ6kYCS+VTCzi6BUuDzU0wrwXyf5uDPArtlofn2AG6aTMiPmN3C909rsEWMNqJqhIVPGP3Exg==`)
    cookieStore = securecookie.New(cookieStoreKey, nil)
}

type cookieStorer struct {
    w http.ResponseWriter
    r *http.Request
}

func NewCookieStorer(w http.ResponseWriter, r *http.Request) authboss.ClientStorer {
    return &cookieStorer{w, r}
}

func (s cookieStorer) Get(key string) (string, bool) {
    cookie, err := s.r.Cookie(key)
    if err != nil {
        return "", false
    }

    var value string
    err = cookieStore.Decode(key, cookie.Value, &value)
    return value, err == nil
}

func (s cookieStorer) Put(key, value string) {
    encoded, err := cookieStore.Encode(key, value)
    if err != nil {
        return
    }

    cookie := &http.Cookie{
        Expires: time.Now().UTC().AddDate(1, 0, 0),
        Name:    key,
        Value:   encoded,
        Path:    "/",
    }
    http.SetCookie(s.w, cookie)
}

func (s cookieStorer) Del(key string) {
    cookie := &http.Cookie{
        MaxAge: -1,
        Name:   key,
        Path:   "/",
    }
    http.SetCookie(s.w, cookie)
}

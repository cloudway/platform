package auth

import (
    "net/http"
    "encoding/base64"
    "gopkg.in/authboss.v0"
    "github.com/gorilla/sessions"
)

const sessionCookieName = "cloudway"

var sessionStore *sessions.CookieStore

func init() {
    sessionStoreKey, _ := base64.StdEncoding.DecodeString(`AbfYwmmt8UCwUuhd9qvfNA9UCuN1cVcKJN1ofbiky6xCyyBj20whe40rJa3Su0WOWLWcPpO1taqJdsEI/65+JA==`)
    sessionStore = sessions.NewCookieStore(sessionStoreKey)
}

type sessionStorer struct {
    w http.ResponseWriter
    r *http.Request
}

func NewSessionStorer(w http.ResponseWriter, r *http.Request) authboss.ClientStorer {
    return &sessionStorer{w, r}
}

func (s sessionStorer) Get(key string) (string, bool) {
    session, err := sessionStore.Get(s.r, sessionCookieName)
    if err != nil {
        return "", false
    }

    strInf, ok := session.Values[key]
    if !ok {
        return "", false
    }

    str, ok := strInf.(string)
    if !ok {
        return "", false
    }

    return str, true
}

func (s sessionStorer) Put(key, value string) {
    session, err := sessionStore.Get(s.r, sessionCookieName)
    if err != nil {
        return
    }

    session.Values[key] = value
    session.Save(s.r, s.w)
}

func (s sessionStorer) Del(key string) {
    session, err := sessionStore.Get(s.r, sessionCookieName)
    if err != nil {
        return
    }

    delete(session.Values, key)
    session.Save(s.r, s.w)
}

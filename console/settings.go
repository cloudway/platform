package console

import (
	"errors"
	"net/http"
	"regexp"

	"github.com/Sirupsen/logrus"
	"github.com/gorilla/mux"
)

func (con *Console) initSettingsRoutes(gets *mux.Router, posts *mux.Router) {
	gets.HandleFunc("/settings", con.settings)
	posts.HandleFunc("/settings/namespace", con.createNamespace)
	posts.HandleFunc("/settings/namespace/delete", con.removeNamespace)
	gets.HandleFunc("/settings/sshkey", con.addkey)
	posts.HandleFunc("/settings/sshkey", con.savekey)
	posts.HandleFunc("/settings/sshkey/delete", con.delkey)
}

func (con *Console) settings(w http.ResponseWriter, r *http.Request) {
	user := con.currentUser(w, r)
	if user == nil {
		return
	}

	data := con.layoutUserData(w, r, user)
	if user.Namespace != "" {
		keys, err := con.SCM.ListKeys(user.Namespace)
		if err != nil {
			logrus.Error(err)
			w.WriteHeader(http.StatusInternalServerError)
			return
		}
		data.MergeKV("sshkeys", keys)
	}
	con.mustRender(w, r, "settings", data)
}

var namespacePattern = regexp.MustCompile("^[a-z][a-z_0-9]*$")

func (con *Console) createNamespace(w http.ResponseWriter, r *http.Request) {
	user := con.currentUser(w, r)
	if user == nil {
		return
	}

	err := r.ParseForm()
	if err == nil {
		namespace := r.PostForm.Get("namespace")
		if !namespacePattern.MatchString(namespace) {
			err = errors.New("名字空间名称只能包含小写英文字母、数字、或者下划线")
		} else {
			err = con.NewUserBroker(user).CreateNamespace(namespace)
		}
	}

	if err != nil {
		data := con.layoutUserData(w, r, user)
		data.MergeKV("error", err)
		con.mustRender(w, r, "settings", data)
		return
	}

	http.Redirect(w, r, "/settings", http.StatusFound)
}

func (con *Console) removeNamespace(w http.ResponseWriter, r *http.Request) {
	user := con.currentUser(w, r)
	if user == nil {
		return
	}

	err := con.NewUserBroker(user).RemoveNamespace()
	if con.badRequest(w, r, err, "/settings") {
		return
	}

	http.Redirect(w, r, "/settings", http.StatusFound)
}

func (con *Console) addkey(w http.ResponseWriter, r *http.Request) {
	user := con.currentUser(w, r)
	if user != nil {
		data := con.layoutUserData(w, r, user)
		con.mustRender(w, r, "addkey", data)
	}
}

func (con *Console) savekey(w http.ResponseWriter, r *http.Request) {
	user := con.currentUser(w, r)
	if user == nil {
		return
	}

	err := r.ParseForm()
	if err == nil {
		content := r.PostForm.Get("content")
		err = con.SCM.AddKey(user.Namespace, content)
	}

	if err != nil {
		data := con.layoutUserData(w, r, user)
		data.MergeKV("error", err)
		con.mustRender(w, r, "addkey", data)
		return
	}

	http.Redirect(w, r, "/settings", http.StatusFound)
}

func (con *Console) delkey(w http.ResponseWriter, r *http.Request) {
	user := con.currentUser(w, r)
	if user == nil {
		return
	}

	key := r.FormValue("key")
	err := con.SCM.RemoveKey(user.Namespace, key)
	if con.badRequest(w, r, err, "/settings") {
		return
	}

	http.Redirect(w, r, "/settings", http.StatusFound)
}

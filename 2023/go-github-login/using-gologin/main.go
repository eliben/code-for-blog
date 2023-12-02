// Users github.com/dghubble/gologin to simplify the OAuth 2 flow for GitHub
// auth.
package main

import (
	"fmt"
	"log"
	"net/http"
	"os"
	"time"

	"github.com/dghubble/gologin/v2"
	"github.com/dghubble/gologin/v2/github"
	"golang.org/x/oauth2"
	githubOAuth2 "golang.org/x/oauth2/github"
)

// These should be taken from your GitHub application settings
// at https://github.com/settings/developers
var GithubClientID = os.Getenv("GITHUB_CLIENT_ID")
var GithubClientSecret = os.Getenv("GITHUB_CLIENT_SECRET")

func main() {
	if len(GithubClientID) == 0 || len(GithubClientSecret) == 0 {
		log.Fatal("Set GITHUB_CLIENT_* env vars")
	}

	addr := "localhost:8080"
	callbackPath := "/github/callback/"

	conf := &oauth2.Config{
		ClientID:     GithubClientID,
		ClientSecret: GithubClientSecret,
		Scopes:       []string{},
		Endpoint:     githubOAuth2.Endpoint,
	}

	cookieConf := gologin.CookieConfig{
		Name:     "state",
		Path:     "/",
		MaxAge:   int(time.Hour.Seconds()),
		Secure:   false, // set to true when serving TLS
		HTTPOnly: true,
	}

	http.HandleFunc("/", rootHandler)

	loginHandler := github.LoginHandler(conf, nil)
	http.Handle("/login/", github.StateHandler(cookieConf, loginHandler))

	callbackHandler := github.CallbackHandler(conf, http.HandlerFunc(githubCallbackHandler), nil)
	http.Handle(callbackPath, github.StateHandler(cookieConf, callbackHandler))

	fmt.Printf("Listening on: http://%s\n", addr)
	log.Panic(http.ListenAndServe(addr, nil))
}

const rootHTML = `
<h1>My web app</h1>
<p>Using gologin</p>
<p>You can log into this app with your GitHub credentials:</p>
<p><a href="/login/">Log in with GitHub</a></p>
`

func rootHandler(w http.ResponseWriter, r *http.Request) {
	fmt.Fprint(w, rootHTML)
}

func githubCallbackHandler(w http.ResponseWriter, r *http.Request) {
	ctx := r.Context()
	githubUser, err := github.UserFromContext(ctx)
	if err != nil {
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}

	w.Header().Set("Content-type", "application/json")
	fmt.Fprint(w, githubUser.String())
}

// Uses golang.org/x/oauth2/github to simplify the workflow of oauth2 with
// GitHub.
package main

import (
	"context"
	"crypto/rand"
	"encoding/base64"
	"fmt"
	"io"
	"log"
	"net/http"
	"os"
	"time"

	"golang.org/x/oauth2"
	"golang.org/x/oauth2/github"
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

	// Note: github auth doesn't support PKCE verification
	// See https://www.rfc-editor.org/rfc/rfc7636.html for what it is
	conf := &oauth2.Config{
		ClientID:     GithubClientID,
		ClientSecret: GithubClientSecret,
		Scopes:       []string{},
		Endpoint:     github.Endpoint,
	}
	lf := &loginFlow{
		conf: conf,
	}

	http.HandleFunc("/", lf.rootHandler)
	http.HandleFunc("/login/", lf.githubLoginHandler)
	http.HandleFunc(callbackPath, lf.githubCallbackHandler)

	fmt.Printf("Listening on: http://%s\n", addr)
	log.Panic(http.ListenAndServe(addr, nil))
}

const rootHTML = `
<h1>My web app</h1>
<p>Using the x/oauth2 package</p>
<p>You can log into this app with your GitHub credentials:</p>
<p><a href="/login/">Log in with GitHub</a></p>
`

type loginFlow struct {
	conf *oauth2.Config
}

func (lf *loginFlow) rootHandler(w http.ResponseWriter, r *http.Request) {
	fmt.Fprint(w, rootHTML)
}

func (lf *loginFlow) githubLoginHandler(w http.ResponseWriter, r *http.Request) {
	state, err := randString(16)
	if err != nil {
		panic(err)
	}
	setShortCookie(w, r, "state", state)

	redirectURL := lf.conf.AuthCodeURL(state, oauth2.AccessTypeOffline)
	http.Redirect(w, r, redirectURL, 301)
}

func (lf *loginFlow) githubCallbackHandler(w http.ResponseWriter, r *http.Request) {
	state, err := r.Cookie("state")
	if err != nil {
		http.Error(w, "state not found", http.StatusBadRequest)
		return
	}
	if r.URL.Query().Get("state") != state.Value {
		http.Error(w, "state did not match", http.StatusBadRequest)
		return
	}

	code := r.URL.Query().Get("code")
	tok, err := lf.conf.Exchange(context.Background(), code)
	if err != nil {
		log.Fatal(err)
	}

	// This client will have a bearer token to access the GitHub API on
	// the user's behalf.
	client := lf.conf.Client(context.Background(), tok)
	resp, err := client.Get("https://api.github.com/user")
	if err != nil {
		panic(err)
	}
	respbody, _ := io.ReadAll(resp.Body)
	userInfo := string(respbody)

	w.Header().Set("Content-type", "application/json")
	fmt.Fprint(w, string(userInfo))
}

// randString generates a random string of length n and returns its
// base64-encoded version.
func randString(n int) (string, error) {
	buf := make([]byte, n)
	if _, err := rand.Read(buf); err != nil {
		return "", err
	}
	return base64.RawURLEncoding.EncodeToString(buf), nil
}

// setShortCookie sets a short-duration cookie with the given name and value
// in the response to the client.
func setShortCookie(w http.ResponseWriter, r *http.Request, name, value string) {
	c := &http.Cookie{
		Name:     name,
		Value:    value,
		Path:     "/",
		MaxAge:   int(time.Hour.Seconds()),
		Secure:   r.TLS != nil,
		HttpOnly: true,
	}
	http.SetCookie(w, c)
}

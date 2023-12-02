// Follows the GitHub OAuth app web application flow:
// https://docs.github.com/en/apps/oauth-apps/building-oauth-apps/authorizing-oauth-apps#web-application-flow
//
// ... without using 3rd party libraries.
// Follow "Step NN" comments in the code.
//
// There's also a GH tutorial for this on:
// https://docs.github.com/en/apps/creating-github-apps/writing-code-for-a-github-app/building-a-login-with-github-button-with-a-github-app
//
// Originally taken from https://sharmarajdaksh.github.io/blog/github-oauth-with-go
// but extensively rewritten.
package main

import (
	"bytes"
	"crypto/rand"
	"encoding/base64"
	"encoding/json"
	"fmt"
	"io"
	"log"
	"net/http"
	"os"
	"time"
)

var GithubClientID = os.Getenv("GITHUB_CLIENT_ID")
var GithubClientSecret = os.Getenv("GITHUB_CLIENT_SECRET")

func main() {
	if len(GithubClientID) == 0 || len(GithubClientSecret) == 0 {
		log.Fatal("Set GITHUB_CLIENT_* env vars")
	}

	http.HandleFunc("/", rootHandler)
	http.HandleFunc("/login/", githubLoginHandler)
	http.HandleFunc("/github/callback/", githubCallbackHandler)

	addr := "localhost:8080"
	fmt.Printf("Listening on: http://%s\n", addr)
	log.Panic(http.ListenAndServe(addr, nil))
}

const rootHTML = `
<h1>My web app</h1>
<p>You can log into this app with your GitHub credentials:</p>
<p><a href="/login/">Log in with GitHub</a></p>
`

func rootHandler(w http.ResponseWriter, r *http.Request) {
	fmt.Fprint(w, rootHTML)
}

func githubLoginHandler(w http.ResponseWriter, r *http.Request) {
	// Step 1: Request a user's GitHub identity. We're not setting
	// redirect_uri, leaving it to GitHub to use the default we set
	// for this application: /github/callback
	//
	// We're setting a random state cookie for the client to return
	// to us when the call comes back, to prevent CSRF.
	state, err := randString(16)
	if err != nil {
		panic(err)
	}
	setShortCookie(w, r, "state", state)
	redirectURL := fmt.Sprintf("https://github.com/login/oauth/authorize?client_id=%s&state=%s", GithubClientID, state)
	http.Redirect(w, r, redirectURL, 301)
}

func githubCallbackHandler(w http.ResponseWriter, r *http.Request) {
	// Step 2: Users are redirected back to your site by GitHub
	// The user is authenticated w/ GitHub by this point.
	//
	// Start by checking the state returned by GitHub matches what
	// we've stored in the cookie.
	state, err := r.Cookie("state")
	if err != nil {
		http.Error(w, "state not found", http.StatusBadRequest)
		return
	}
	if r.URL.Query().Get("state") != state.Value {
		http.Error(w, "state did not match", http.StatusBadRequest)
		return
	}

	// GH provides us a code we can use to ask for information about the user.
	code := r.URL.Query().Get("code")

	// We use the code, alongside our client ID and secret to ask GH for an
	// access token to the API.
	requestBodyMap := map[string]string{
		"client_id":     GithubClientID,
		"client_secret": GithubClientSecret,
		"code":          code,
	}
	requestJSON, err := json.Marshal(requestBodyMap)
	if err != nil {
		panic(err)
	}

	req, err := http.NewRequest("POST", "https://github.com/login/oauth/access_token", bytes.NewBuffer(requestJSON))
	if err != nil {
		panic(err)
	}
	req.Header.Set("Content-Type", "application/json")
	req.Header.Set("Accept", "application/json")

	resp, err := http.DefaultClient.Do(req)
	if err != nil {
		http.Error(w, "unable to connect to access_token endpoint", http.StatusInternalServerError)
		return
	}

	respbody, _ := io.ReadAll(resp.Body)

	// Represents the response received from Github
	var ghresp struct {
		AccessToken string `json:"access_token"`
		TokenType   string `json:"token_type"`
		Scope       string `json:"scope"`
	}
	json.Unmarshal(respbody, &ghresp)

	// Step 3: Use the access token to access the API and present
	// information to the user.
	userInfo := getGitHubUserInfo(ghresp.AccessToken)

	w.Header().Set("Content-type", "application/json")
	fmt.Fprint(w, string(userInfo))
}

// getGitHubUserInfo queries GitHub's user API for information about the
// authorized user, given the access token received earlier.
func getGitHubUserInfo(accessToken string) string {
	// Query the GH API for user info
	req, err := http.NewRequest("GET", "https://api.github.com/user", nil)
	if err != nil {
		panic(err)
	}
	req.Header.Set("Authorization", "Bearer "+accessToken)

	resp, err := http.DefaultClient.Do(req)
	if err != nil {
		panic(err)
	}

	respbody, _ := io.ReadAll(resp.Body)
	return string(respbody)
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

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
	"encoding/json"
	"fmt"
	"io/ioutil"
	"log"
	"net/http"
	"os"
)

var GithubClientID = os.Getenv("GITHUB_CLIENT_ID")
var GithubClientSecret = os.Getenv("GITHUB_CLIENT_SECRET")

func main() {
	http.HandleFunc("/", rootHandler)
	http.HandleFunc("/login/github/", githubLoginHandler)
	http.HandleFunc("/github/callback/", githubCallbackHandler)

	// Route where the authenticated user is redirected to
	http.HandleFunc("/loggedin", func(w http.ResponseWriter, r *http.Request) {
		loggedinHandler(w, r, "")
	})

	addr := "localhost:8080"
	fmt.Printf("Listening on: http://%s\n", addr)
	log.Panic(http.ListenAndServe(addr, nil))
}

const rootHTML = `
<h1>My web app</h1>
<p>You can log into this app with your GitHub credentials:</p>
<p><a href="/login/github/">Log in with GitHub</a></p>
`

func rootHandler(w http.ResponseWriter, r *http.Request) {
	fmt.Fprint(w, rootHTML)
}

func githubLoginHandler(w http.ResponseWriter, r *http.Request) {
	// Step 1: Request a user's GitHub identity. We're not setting
	// redirect_uri, leaving it to GitHub to use the default we set
	// for this application: /github/callback
	redirectURL := fmt.Sprintf("https://github.com/login/oauth/authorize?client_id=%s", GithubClientID)
	http.Redirect(w, r, redirectURL, 301)
}

func githubCallbackHandler(w http.ResponseWriter, r *http.Request) {
	// Step 2: Users are redirected back to your site by GitHub
	// The user is authenticated w/ GitHub by this point.
	// GH provides us a code we can use to ask for information about the user.
	code := r.URL.Query().Get("code")

	// We use the code, alongside our client ID and secret to ask GH for an
	// access token to the API.
	requestBodyMap := map[string]string{
		"client_id":     GithubClientID,
		"client_secret": GithubClientSecret,
		"code":          code,
	}
	requestJSON, _ := json.Marshal(requestBodyMap)

	req, reqerr := http.NewRequest("POST", "https://github.com/login/oauth/access_token", bytes.NewBuffer(requestJSON))
	if reqerr != nil {
		log.Panic("Request creation failed")
	}
	req.Header.Set("Content-Type", "application/json")
	req.Header.Set("Accept", "application/json")

	resp, resperr := http.DefaultClient.Do(req)
	if resperr != nil {
		log.Panic("Request failed")
	}

	respbody, _ := ioutil.ReadAll(resp.Body)

	// Represents the response received from Github
	type githubAccessTokenResponse struct {
		AccessToken string `json:"access_token"`
		TokenType   string `json:"token_type"`
		Scope       string `json:"scope"`
	}

	var ghresp githubAccessTokenResponse
	json.Unmarshal(respbody, &ghresp)

	accessToken := ghresp.AccessToken

	// Step 3: Use the access token to access the API
	githubData := getGithubData(accessToken)
	loggedinHandler(w, r, githubData)
}

func getGithubData(accessToken string) string {
	// Query the GH API for user info
	req, reqerr := http.NewRequest("GET", "https://api.github.com/user", nil)
	if reqerr != nil {
		log.Panic("API Request creation failed")
	}

	authorizationHeaderValue := fmt.Sprintf("Bearer %s", accessToken)
	req.Header.Set("Authorization", authorizationHeaderValue)

	resp, resperr := http.DefaultClient.Do(req)
	if resperr != nil {
		log.Panic("Request failed")
	}

	respbody, _ := ioutil.ReadAll(resp.Body)

	return string(respbody)
}

func loggedinHandler(w http.ResponseWriter, r *http.Request, githubData string) {
	if githubData == "" {
		// Unauthorized users get an unauthorized message
		fmt.Fprintf(w, "UNAUTHORIZED!")
		return
	}

	w.Header().Set("Content-type", "application/json")

	// Prettifying the json
	var prettyJSON bytes.Buffer
	// json.indent is a library utility function to prettify JSON indentation
	parserr := json.Indent(&prettyJSON, []byte(githubData), "", "\t")
	if parserr != nil {
		log.Panic("JSON parse error")
	}

	// Return the prettified JSON as a string
	fmt.Fprintf(w, string(prettyJSON.Bytes()))
}

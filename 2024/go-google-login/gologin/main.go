// Uses github.com/dghubble/gologin
//
// Eli Bendersky [https://eli.thegreenplace.net]
// This code is in the public domain.
package main

import (
	"encoding/json"
	"fmt"
	"log"
	"net/http"
	"os"

	"github.com/dghubble/gologin/v2"
	"github.com/dghubble/gologin/v2/google"
	"golang.org/x/oauth2"
	oauth2google "golang.org/x/oauth2/google"
)

// These should be taken from https://console.cloud.google.com/apis/credentials
var GoogleClientID = os.Getenv("GOOGLE_CLIENT_ID")
var GoogleClientSecret = os.Getenv("GOOGLE_CLIENT_SECRET")

const servingSchema = "http://"
const servingAddress = "localhost:8080"
const callbackPath = "/google/callback"

func main() {
	if len(GoogleClientID) == 0 || len(GoogleClientSecret) == 0 {
		log.Fatal("Set GOOGLE_CLIENT_* env vars")
	}

	conf := &oauth2.Config{
		ClientID:     GoogleClientID,
		ClientSecret: GoogleClientSecret,
		RedirectURL:  servingSchema + servingAddress + callbackPath,
		Scopes:       []string{"profile", "email"},
		Endpoint:     oauth2google.Endpoint,
	}

	// gologin has a default cookie configuration for debug deployments (no TLS).
	cookieConf := gologin.DebugOnlyCookieConfig
	http.HandleFunc("/", rootHandler)

	loginHandler := google.LoginHandler(conf, nil)
	http.Handle("/login/", google.StateHandler(cookieConf, loginHandler))

	callbackHandler := google.CallbackHandler(conf, http.HandlerFunc(googleCallbackHandler), nil)
	http.Handle(callbackPath, google.StateHandler(cookieConf, callbackHandler))

	fmt.Printf("Listening on: %s%s\n", servingSchema, servingAddress)
	log.Panic(http.ListenAndServe(servingAddress, nil))
}

const rootHTML = `
<h1>My web app</h1>
<p>Using gologin</p>
<p>You can log into this app with your Google credentials:</p>
<p><a href="/login/">Log in with Google</a></p>
`

func rootHandler(w http.ResponseWriter, r *http.Request) {
	fmt.Fprint(w, rootHTML)
}

func googleCallbackHandler(w http.ResponseWriter, r *http.Request) {
	ctx := r.Context()
	googleUser, err := google.UserFromContext(ctx)
	if err != nil {
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}

	buf, _ := json.Marshal(googleUser)
	fmt.Println(string(buf))
}

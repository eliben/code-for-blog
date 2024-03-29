// The flow here follows the documentation page at
// https://developers.google.com/identity/openid-connect/openid-connect
//
// Eli Bendersky [https://eli.thegreenplace.net]
// This code is in the public domain.
package main

import (
	"crypto/rand"
	"encoding/base64"
	"encoding/json"
	"fmt"
	"io"
	"log"
	"net/http"
	"os"
	"time"

	"github.com/coreos/go-oidc/v3/oidc"
	"golang.org/x/net/context"
	"golang.org/x/oauth2"
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

	// Set up oauth2 package configuration.
	provider, err := oidc.NewProvider(context.Background(), "https://accounts.google.com")
	if err != nil {
		log.Fatal(err)
	}
	config := &oauth2.Config{
		ClientID:     GoogleClientID,
		ClientSecret: GoogleClientSecret,
		Endpoint:     provider.Endpoint(),
		RedirectURL:  servingSchema + servingAddress + callbackPath,
		Scopes:       []string{oidc.ScopeOpenID, "profile", "email"},
	}
	lf := &loginFlow{
		conf:     config,
		provider: provider,
	}

	mux := http.NewServeMux()
	mux.HandleFunc("/", lf.rootHandler)
	mux.HandleFunc(callbackPath, lf.callbackHandler)

	log.Printf("Listening on: %s%s\n", servingSchema, servingAddress)
	log.Panic(http.ListenAndServe(servingAddress, mux))
}

type loginFlow struct {
	conf     *oauth2.Config
	provider *oidc.Provider
}

func (lf *loginFlow) rootHandler(w http.ResponseWriter, req *http.Request) {
	// These steps (and the ones in callbackHandler) follow the flow outlined in
	// https://developers.google.com/identity/openid-connect/openid-connect
	//
	// 1. Create an anti-forgery state token and save it in a cookie.
	state, err := randString(16)
	if err != nil {
		http.Error(w, "Internal error", http.StatusInternalServerError)
		return
	}

	c := &http.Cookie{
		Name:     "state",
		Value:    state,
		MaxAge:   int(time.Hour.Seconds()),
		Secure:   req.TLS != nil,
		HttpOnly: true,
	}
	http.SetCookie(w, c)

	// 2. Send an authentication request to Google by redirecting the user to
	//    Google's auth endpoint.
	http.Redirect(w, req, lf.conf.AuthCodeURL(state), http.StatusFound)
}

func (lf *loginFlow) callbackHandler(w http.ResponseWriter, req *http.Request) {
	// 3. Confirm anti-forgery state token.
	state, err := req.Cookie("state")
	if err != nil {
		http.Error(w, "state not found", http.StatusBadRequest)
		return
	}
	if req.URL.Query().Get("state") != state.Value {
		http.Error(w, "state did not match", http.StatusBadRequest)
		return
	}

	// 4. Exchange code for access token and ID token.
	oauth2Token, err := lf.conf.Exchange(context.Background(), req.URL.Query().Get("code"))
	if err != nil {
		http.Error(w, "Failed to exchange token: "+err.Error(), http.StatusInternalServerError)
		return
	}

	// 5. Obtain user information from the ID token.
	userInfo, err := lf.provider.UserInfo(context.Background(), oauth2.StaticTokenSource(oauth2Token))
	if err != nil {
		http.Error(w, "Failed to get userinfo: "+err.Error(), http.StatusInternalServerError)
		return
	}

	b, err := json.MarshalIndent(userInfo, "", "    ")
	if err != nil {
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}
	fmt.Println(string(b))
}

// randString generates a random string of length n and returns its
// base64-encoded version.
func randString(nByte int) (string, error) {
	b := make([]byte, nByte)
	if _, err := io.ReadFull(rand.Reader, b); err != nil {
		return "", err
	}
	return base64.RawURLEncoding.EncodeToString(b), nil
}

// The flow here follows the documentation page at
// https://developers.google.com/identity/gsi/web/guides/verify-google-id-token
//
// Eli Bendersky [https://eli.thegreenplace.net]
// This code is in the public domain.
package main

import (
	"context"
	"fmt"
	"html/template"
	"log"
	"net/http"
	"os"

	"google.golang.org/api/idtoken"
)

// This should be taken from https://console.cloud.google.com/apis/credentials
var GoogleClientID = os.Getenv("GOOGLE_CLIENT_ID")

const servingSchema = "http://"
const servingAddress = "localhost:8080"
const callbackPath = "/google/callback"

var rootHtmlTemplate = template.Must(template.New("root").Parse(`
<!DOCTYPE html>
<html>
<body>
    <script src="https://accounts.google.com/gsi/client" async></script>

		<h1>Welcome to this web app!</h1>
		<p>Let's sign in with Google:</p>
    <div
        id="g_id_onload"
        data-client_id="{{.ClientID}}"
        data-login_uri="{{.CallbackUrl}}">
    </div>
    <div
        class="g_id_signin"
        data-type="standard"
        data-theme="filled_blue"
        data-text="sign_in_with"
        data-shape="rectangular"
				data-width="200"
        data-logo_alignment="left">
    </div>
</body>
</html>
`))

func main() {
	if len(GoogleClientID) == 0 {
		log.Fatal("Set GOOGLE_CLIENT_ID env var")
	}

	mux := http.NewServeMux()
	mux.HandleFunc("/", rootHandler)
	mux.HandleFunc(callbackPath, callbackHandler)

	log.Printf("Listening on: %s%s\n", servingSchema, servingAddress)
	log.Panic(http.ListenAndServe(servingAddress, mux))
}

func rootHandler(w http.ResponseWriter, req *http.Request) {
	err := rootHtmlTemplate.Execute(w, map[string]string{
		"CallbackUrl": servingSchema + servingAddress + callbackPath,
		"ClientID":    GoogleClientID,
	})
	if err != nil {
		panic(err)
	}
}

func callbackHandler(w http.ResponseWriter, req *http.Request) {
	defer req.Body.Close()

	if err := req.ParseForm(); err != nil {
		http.Error(w, err.Error(), http.StatusBadRequest)
		return
	}

	// The following steps follow
	// https://developers.google.com/identity/gsi/web/guides/verify-google-id-token
	//
	// 1. Verify the CSRF token, which uses the double-submit-cookie pattern and
	//    is added both as a cookie value and post body.
	token, err := req.Cookie("g_csrf_token")
	if err != nil {
		http.Error(w, "token not found", http.StatusBadRequest)
		return
	}

	bodyToken := req.FormValue("g_csrf_token")
	if token.Value != bodyToken {
		http.Error(w, "token mismatch", http.StatusBadRequest)
	}

	// 2. Verify the ID token, which is returned in the `credential` field.
	//    We use the idtoken package for this. `audience` is our client ID.
	ctx := context.Background()
	validator, err := idtoken.NewValidator(ctx)
	if err != nil {
		panic(err)
	}
	credential := req.FormValue("credential")
	payload, err := validator.Validate(ctx, credential, GoogleClientID)
	if err != nil {
		http.Error(w, err.Error(), http.StatusBadRequest)
	}

	// 3. Once the token's validity is confirmed, we can use the user identifying
	//    information in the Google ID token.
	for k, v := range payload.Claims {
		fmt.Printf("%v: %v\n", k, v)
	}
}

package main

import (
	"context"
	"fmt"
	"html/template"
	"log"
	"net/http"
	"net/http/httputil"
	"os"

	"google.golang.org/api/idtoken"
)

// This should be taken from https://console.cloud.google.com/apis/credentials
var GoogleClientID = os.Getenv("GOOGLE_CLIENT_ID")

const servingAddress = "localhost:8080"
const servingSchema = "http://"
const callbackPath = "/google/callback"

var homeHtmlTemplate = template.Must(template.New("home").Parse(`
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
	mux.HandleFunc("/", homeHandler)
	mux.HandleFunc(callbackPath, callbackHandler)

	log.Printf("Starting Server listening on %s%s\n", servingSchema, servingAddress)
	err := http.ListenAndServe(servingAddress, mux)
	if err != nil {
		log.Fatal("ListenAndServe: ", err)
	}
}

func homeHandler(w http.ResponseWriter, req *http.Request) {
	err := homeHtmlTemplate.Execute(w, map[string]string{
		"CallbackUrl": servingSchema + servingAddress + callbackPath,
		"ClientID":    GoogleClientID,
	})
	if err != nil {
		panic(err)
	}
}

func callbackHandler(w http.ResponseWriter, req *http.Request) {
	b, err := httputil.DumpRequest(req, false)
	if err != nil {
		panic(err)
	}
	fmt.Println(string(b))
	defer req.Body.Close()

	if err := req.ParseForm(); err != nil {
		http.Error(w, err.Error(), http.StatusBadRequest)
		return
	}

	token, err := req.Cookie("g_csrf_token")
	if err != nil {
		http.Error(w, "token not found", http.StatusBadRequest)
		return
	}

	bodyToken := req.FormValue("g_csrf_token")
	if token.Value != bodyToken {
		http.Error(w, "token mismatch", http.StatusBadRequest)
	}

	ctx := context.Background()
	validator, err := idtoken.NewValidator(ctx)
	if err != nil {
		panic(err)
	}
	credential := req.FormValue("credential")
	claims, err := validator.Validate(ctx, credential, GoogleClientID)
	if err != nil {
		http.Error(w, err.Error(), http.StatusBadRequest)
	}

	for k, v := range claims.Claims {
		fmt.Printf("%v: %v\n", k, v)
	}
}

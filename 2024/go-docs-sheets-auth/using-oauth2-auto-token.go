// Example of accessing the Google Sheets API from Go using OAuth 2 for auth.
//
// Follow the steps in the GCP Go quickstart
// (https://developers.google.com/docs/api/quickstart/go) for obtaining a
// credentials file.
//
// Eli Bendersky [https://eli.thegreenplace.net]
// This code is in the public domain.
package main

import (
	"context"
	"encoding/json"
	"flag"
	"fmt"
	"log"
	"net"
	"net/http"
	"os"

	"golang.org/x/oauth2"
	"golang.org/x/oauth2/google"
	"google.golang.org/api/option"
	"google.golang.org/api/sheets/v4"
)

// makeOauthClient creates a new http.Client with oauth2 set up from the
// given config.
func makeOauthClient(config *oauth2.Config) *http.Client {
	tokFile := "token.json"
	tok, err := loadCachedToken(tokFile)
	if err != nil {
		tok = getTokenFromWeb(config)
		saveCachedToken(tokFile, tok)
	}
	return config.Client(context.Background(), tok)
}

// authenticateUser launches a web browser to authenticate the user vs. Google's
// auth server and returns the auth code that can then be exchanged for tokens.
func authenticateUser(config *oauth2.Config) string {
	const redirectPath = "/redirect"
	// We spin up a goroutine with a web server listening on the redirect route,
	// which the auth server will redirect the user's browser to after
	// authentication.
	listener, err := net.Listen("tcp", ":0")
	if err != nil {
		log.Fatal(err)
	}
	port := listener.Addr().(*net.TCPAddr).Port

	// When the web server receives redirection, it sends the code to codeChan.
	codeChan := make(chan string)
	var srv http.Server

	go func() {
		mux := http.NewServeMux()
		mux.HandleFunc(redirectPath, func(w http.ResponseWriter, req *http.Request) {
			codeChan <- req.URL.Query().Get("code")
		})
		srv.Handler = mux
		if err := srv.Serve(listener); err != http.ErrServerClosed {
			log.Fatal(err)
		}
	}()

	config.RedirectURL = fmt.Sprintf("http://localhost:%d%s", port, redirectPath)
	authURL := config.AuthCodeURL("state-token", oauth2.AccessTypeOffline)
	fmt.Println("Click this link to authenticate:\n", authURL)

	// Receive code from the web server and shut it down.
	authCode := <-codeChan
	if err := srv.Shutdown(context.Background()); err != nil {
		log.Fatal(err)
	}

	return authCode
}

func getTokenFromWeb(config *oauth2.Config) *oauth2.Token {
	authCode := authenticateUser(config)
	tok, err := config.Exchange(context.Background(), authCode)
	if err != nil {
		log.Fatalf("unable to retrieve token from web: %v", err)
	}
	return tok
}

// loadCachedToken tries to load a cached token from a local file.
func loadCachedToken(file string) (*oauth2.Token, error) {
	f, err := os.Open(file)
	if err != nil {
		return nil, err
	}
	defer f.Close()
	tok := &oauth2.Token{}
	err = json.NewDecoder(f).Decode(tok)
	return tok, err
}

// saveCachedToken saves an oauth2 token to a local file.
func saveCachedToken(path string, token *oauth2.Token) {
	fmt.Printf("Saving token to: %s\n", path)
	f, err := os.OpenFile(path, os.O_RDWR|os.O_CREATE|os.O_TRUNC, 0600)
	defer f.Close()
	if err != nil {
		log.Fatalf("unable to cache OAuth token: %v", err)
	}
	json.NewEncoder(f).Encode(token)
}

func main() {
	credFilePath := flag.String("credfile", "", "path to the credentials file")
	flag.Parse()

	ctx := context.Background()
	b, err := os.ReadFile(*credFilePath)
	if err != nil {
		log.Fatalf("unable to read client secret file: %v", err)
	}

	// If modifying these scopes, delete your previously saved token.json.
	scopes := []string{
		"https://www.googleapis.com/auth/spreadsheets.readonly",
	}
	config, err := google.ConfigFromJSON(b, scopes...)
	if err != nil {
		log.Fatalf("unable to parse client secret file to config: %v", err)
	}
	client := makeOauthClient(config)

	srv, err := sheets.NewService(ctx, option.WithHTTPClient(client))
	if err != nil {
		log.Fatalf("unable to retrieve Sheets service: %v", err)
	}

	// Full doc link (to my "testsheet2" sheet)
	// https://docs.google.com/spreadsheets/d/1qsNWsZuw98r9HEl01vwxCO5O1sIsI-fr0bJ4KGVvWsU/
	// Note: if access is restricted, the service account's email address should
	// be given explicit view access to the sheet. The email is taken from the
	// service account's GCP IAM page (Details tab).
	docId := "1qsNWsZuw98r9HEl01vwxCO5O1sIsI-fr0bJ4KGVvWsU"
	doc, err := srv.Spreadsheets.Get(docId).Do()
	if err != nil {
		log.Fatalf("unable to retrieve data from document: %v", err)
	}
	fmt.Printf("The title of the doc is: %s\n", doc.Properties.Title)

	val, err := srv.Spreadsheets.Values.Get(docId, "Sheet1!A:B").Do()
	if err != nil {
		log.Fatalf("unable to retrieve range from document: %v", err)
	}

	fmt.Printf("Selected major dimension=%v, range=%v\n", val.MajorDimension, val.Range)
	for _, row := range val.Values {
		fmt.Println(row)
	}
}

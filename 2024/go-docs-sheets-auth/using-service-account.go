// Example of accessing the Google Sheets API from Go using a service account
// for auth.
//
// Create a new service account at https://console.cloud.google.com/iam-admin/serviceaccounts
// in your project, and add a new key in "Manage keys" for it.
// Copy the resulting JSON file locally, and provide its path with the
// -keyfile flag.
//
// Eli Bendersky [https://eli.thegreenplace.net]
// This code is in the public domain.
package main

import (
	"context"
	"flag"
	"fmt"
	"io/ioutil"
	"log"

	"golang.org/x/oauth2/google"
	"google.golang.org/api/option"
	"google.golang.org/api/sheets/v4"
)

func main() {
	keyFilePath := flag.String("keyfile", "", "path to the credentials file")
	flag.Parse()

	ctx := context.Background()
	credentials, err := ioutil.ReadFile(*keyFilePath)
	if err != nil {
		log.Fatal("unable to read key file:", err)
	}

	scopes := []string{
		"https://www.googleapis.com/auth/spreadsheets.readonly",
	}
	config, err := google.JWTConfigFromJSON(credentials, scopes...)
	if err != nil {
		log.Fatal("unable to create JWT configuration:", err)
	}

	srv, err := sheets.NewService(ctx, option.WithHTTPClient(config.Client(ctx)))
	if err != nil {
		log.Fatalf("unable to retrieve sheets service: %v", err)
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

// Example of accessing the Google Sheets API from Go using Application
// Default Credentials.
//
// This assumes that you ran:
//
//	$ gcloud auth application-default login --scopes=openid,https://www.googleapis.com/auth/userinfo.email,https://www.googleapis.com/auth/cloud-platform,https://www.googleapis.com/auth/spreadsheets
//
// Also, the sheets API has to be enabled for your project:
//
//	$ gcloud services enable sheets.googleapis.com --project=<PROJECT-NAME>
//
// To see all enabled APIs:
//
//	$ gcloud services list --enabled --project=<PROJECT-NAME>
//
// Eli Bendersky [https://eli.thegreenplace.net]
// This code is in the public domain.
package main

import (
	"context"
	"fmt"
	"log"

	"google.golang.org/api/option"
	"google.golang.org/api/sheets/v4"
)

func main() {
	ctx := context.Background()

	scopes := []string{
		"https://www.googleapis.com/auth/spreadsheets.readonly",
	}
	srv, err := sheets.NewService(ctx, option.WithScopes(scopes...))
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

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
	credFilePath := flag.String("credfile", "", "path to the credentials file")
	flag.Parse()

	ctx := context.Background()
	credentials, err := ioutil.ReadFile(*credFilePath)
	if err != nil {
		log.Fatal("unable to read credentials file:", err)
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
		log.Fatalf("Unable to retrieve sheets service: %v", err)
	}

	// Full doc link (to my "testsheet2" sheet)
	// https://docs.google.com/spreadsheets/d/1qsNWsZuw98r9HEl01vwxCO5O1sIsI-fr0bJ4KGVvWsU/
	docId := "1qsNWsZuw98r9HEl01vwxCO5O1sIsI-fr0bJ4KGVvWsU"
	doc, err := srv.Spreadsheets.Get(docId).Do()
	if err != nil {
		log.Fatalf("Unable to retrieve data from document: %v", err)
	}
	fmt.Printf("The title of the doc is: %s\n", doc.Properties.Title)

	val, err := srv.Spreadsheets.Values.Get(docId, "Sheet1!A:B").Do()
	if err != nil {
		log.Fatalf("Unable to retrieve range from document: %v", err)
	}

	fmt.Println(val.MajorDimension, val.Range)
	for _, row := range val.Values {
		fmt.Println(row)
	}
}

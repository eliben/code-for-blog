// An example streaming XML parser using encoding/xml.
//
// Eli Bendersky [http://eli.thegreenplace.net]
// This code is in the public domain.
package main

import (
	"encoding/xml"
	"fmt"
	"io"
	"log"
	"os"
	"strings"
)

type location struct {
	Data string `xml:",chardata"`
}

func main() {
	f, err := os.Open(os.Args[1])
	if err != nil {
		log.Fatal(err)
	}
	defer f.Close()

	d := xml.NewDecoder(f)
	count := 0
	for {
		tok, err := d.Token()
		if tok == nil || err == io.EOF {
			// EOF means we're done.
			break
		} else if err != nil {
			log.Fatalf("Error decoding token: %s", err)
		}

		switch ty := tok.(type) {
		case xml.StartElement:
			if ty.Name.Local == "location" {
				// If this is a start element named "location", parse this element
				// fully.
				var loc location
				if err = d.DecodeElement(&loc, &ty); err != nil {
					log.Fatalf("Error decoding item: %s", err)
				}
				if strings.Contains(loc.Data, "Africa") {
					count++
				}
			}
		default:
		}
	}

	fmt.Println("count =", count)
}

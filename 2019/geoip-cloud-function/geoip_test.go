// Tests for the GeoIP handler.
//
// Eli Bendersky [http://eli.thegreenplace.net]
// This code is in the public domain.
package geoip

import (
	"encoding/json"
	"io/ioutil"
	"net/http/httptest"
	"testing"
)

func TestHelloHTTP(t *testing.T) {
	req := httptest.NewRequest("GET", "/", nil)
	req.Header.Set("X-AppEngine-Region", "ca")

	rr := httptest.NewRecorder()
	GeoIP(rr, req)

	out, err := ioutil.ReadAll(rr.Result().Body)
	if err != nil {
		t.Fatalf("ReadAll: %v", err)
	}

	var gd geoIPData
	err = json.Unmarshal(out, &gd)
	if err != nil {
		t.Fatal(err)
	}
	if gd.Region != "ca" {
		t.Errorf("expected Region 'ca', got %s", gd.Region)
	}
}

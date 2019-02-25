// Basic smoke test for the payloadserver HTTP endpoint (Payload). The secret
// key is not set so the response will be "Signature validation failed", but
// it should be a valid HTTP response.
//
// Eli Bendersky [https://eli.thegreenplace.net]
// This code is in the public domain.
package payloadserver

import (
	"io/ioutil"
	"net/http/httptest"
	"strings"
	"testing"
)

func TestHelloHTTP(t *testing.T) {
	req := httptest.NewRequest("GET", "/", nil)

	rr := httptest.NewRecorder()
	Payload(rr, req)

	out, err := ioutil.ReadAll(rr.Result().Body)
	if err != nil {
		t.Fatalf("ReadAll: %v", err)
	}
	if !strings.Contains(string(out), "Signature validation failed") {
		t.Errorf("Got %s, expected to see 'reply num'", string(out))
	}
}

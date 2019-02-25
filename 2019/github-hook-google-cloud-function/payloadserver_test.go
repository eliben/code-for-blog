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

// Simple payload server for Github hooks. Payload is the http handler.
// For instructions on deploying as a Google Cloud Function, see README.rst
//
// Eli Bendersky [https://eli.thegreenplace.net]
// This code is in the public domain.
package payloadserver

import (
	"crypto/hmac"
	"crypto/sha1"
	"encoding/hex"
	"fmt"
	"io/ioutil"
	"log"
	"net/http"
	"os"
	"strings"
)

// checkMAC reports whether messageMAC is a valid HMAC tag for message.
func checkMAC(message, messageMAC, key []byte) bool {
	mac := hmac.New(sha1.New, key)
	mac.Write(message)
	expectedMAC := mac.Sum(nil)
	return hmac.Equal(messageMAC, expectedMAC)
}

// validateSignature validates the MAC signature in the request header using
// the secret key and the message body. Returns true iff valid.
func validateSignature(body []byte, r *http.Request) bool {
	sigHeader := r.Header["X-Hub-Signature"]
	if len(sigHeader) < 1 {
		log.Println("signature header too short")
		return false
	}
	parts := strings.Split(sigHeader[0], "=")
	if len(parts) != 2 || parts[0] != "sha1" {
		log.Println("Expected signature header 'sha1=XXXXX'")
		return false
	}
	decoded, err := hex.DecodeString(parts[1])
	if err != nil {
		log.Println(err)
		return false
	}

	return checkMAC(body, decoded, []byte(os.Getenv("HOOK_SECRET_KEY")))
}

// Payload is the entry point HTTP handler.
func Payload(w http.ResponseWriter, r *http.Request) {
	body, err := ioutil.ReadAll(r.Body)
	if err != nil {
		log.Println(err)
		return
	}
	log.Println("Header:\n---------")
	fmt.Println(r.Header)

	if !validateSignature(body, r) {
		m := "Signature validation failed"
		log.Println(m)
		w.Write([]byte(m))
		return
	}

	fmt.Println("Body:\n---------")
	log.Println(string(body))
}

// Middleware.
//
// Eli Bendersky [https://eli.thegreenplace.net]
// This code is in the public domain.
package middleware

import (
	"context"
	"net/http"

	"example.com/internal/authdb"
)

// UserContextKey is the key in a request's context used to check if the request
// has an authenticated user. The middleware will set the value of this key to
// the username, if the user was properly authenticated with a password.
const UserContextKey = "user"

func BasicAuth(next http.Handler) http.Handler {
	return http.HandlerFunc(func(w http.ResponseWriter, req *http.Request) {
		user, pass, ok := req.BasicAuth()
		if ok && authdb.VerifyUserPass(user, pass) {
			newctx := context.WithValue(req.Context(), UserContextKey, user)
			next.ServeHTTP(w, req.WithContext(newctx))
		} else {
			w.Header().Set("WWW-Authenticate", `Basic realm="api"`)
			http.Error(w, "Unauthorized", http.StatusUnauthorized)
		}
	})
}

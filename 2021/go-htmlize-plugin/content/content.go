// Package content represents the "model" of the htmlize program, with data
// stored by the program about "posts" and the "database".
//
// Eli Bendersky [https://eli.thegreenplace.net]
// This code is in the public domain.
package content

type DB struct {
	Name string
}

type Post struct {
	Id       int
	Author   string
	Contents string
}

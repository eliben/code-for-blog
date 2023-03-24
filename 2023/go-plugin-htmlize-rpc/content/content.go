// Imaginary Post/DB "contents" for our tool; these serve as data-carrying
// types to pass around.
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

// Simple example of parsing Go source text (from stdin) to an AST and printing
// the AST back (to stdout).
//
// Eli Bendersky [https://eli.thegreenplace.net]
// This code is in the public domain.
package main

import (
	"go/parser"
	"go/printer"
	"go/token"
	"log"
	"os"
)

func main() {
	fset := token.NewFileSet()
	file, err := parser.ParseFile(fset, "src.go", os.Stdin, 0)
	if err != nil {
		log.Fatal(err)
	}

	printer.Fprint(os.Stdout, fset, file)
}

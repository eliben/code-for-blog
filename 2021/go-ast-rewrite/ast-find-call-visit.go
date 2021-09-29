package main

import (
	"fmt"
	"go/ast"
	"go/parser"
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

	visitor := &Visitor{fset: fset}
	ast.Walk(visitor, file)
}

type Visitor struct {
	fset *token.FileSet
}

func (v *Visitor) Visit(n ast.Node) ast.Visitor {
	if n == nil {
		return nil
	}

	switch x := n.(type) {
	case *ast.CallExpr:
		id, ok := x.Fun.(*ast.Ident)
		if ok {
			if id.Name == "pred" {
				fmt.Printf("Visit found call to pred() at %s\n", v.fset.Position(n.Pos()))
			}
		}
	}
	return v
}

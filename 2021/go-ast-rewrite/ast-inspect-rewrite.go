// Apply basic AST rewriting to Go code, without using external packages.
//
// Eli Bendersky [https://eli.thegreenplace.net]
// This code is in the public domain.
package main

import (
	"fmt"
	"go/ast"
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

	ast.Inspect(file, func(n ast.Node) bool {
		switch x := n.(type) {
		case *ast.CallExpr:
			id, ok := x.Fun.(*ast.Ident)
			if ok {
				if id.Name == "pred" {
					id.Name += "2"
				}
			}
		case *ast.FuncDecl:
			if x.Name.Name == "pred" {
				x.Name.Name += "2"
			}

			newCallStmt := &ast.ExprStmt{
				X: &ast.CallExpr{
					Fun: &ast.SelectorExpr{
						X: &ast.Ident{
							Name: "fmt",
						},
						Sel: &ast.Ident{
							Name: "Println",
						},
					},
					Args: []ast.Expr{
						&ast.BasicLit{
							Kind:  token.STRING,
							Value: `"instrumentation"`,
						},
					},
				},
			}

			x.Body.List = append([]ast.Stmt{newCallStmt}, x.Body.List...)
		}

		return true
	})

	fmt.Println("Modified AST:")
	printer.Fprint(os.Stdout, fset, file)
}

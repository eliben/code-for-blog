// Sample Go analysis tool that demonstrates how to find generic instantiations.
//
// Eli Bendersky [https://eli.thegreenplace.net]
// This code is in the public domain.
package main

import (
	"flag"
	"fmt"
	"go/ast"
	"go/token"
	"go/types"
	"log"
	"os"

	"golang.org/x/tools/go/packages"
)

// This var is global for easier AST dumping.
var fset = token.NewFileSet()

func main() {
	const mode packages.LoadMode = packages.NeedName |
		packages.NeedTypes |
		packages.NeedSyntax |
		packages.NeedTypesInfo

	flag.Parse()
	if flag.NArg() != 1 {
		log.Fatal("Expecting a single argument: directory of module")
	}

	cfg := &packages.Config{Fset: fset, Mode: mode, Dir: flag.Args()[0]}
	pkgs, err := packages.Load(cfg, "./...")
	if err != nil {
		log.Fatal(err)
	}

	for _, pkg := range pkgs {
		processPackage(pkg)
	}
}

func processPackage(pkg *packages.Package) {
	if len(pkg.Errors) > 0 {
		for _, e := range pkg.Errors {
			fmt.Printf("Error: %s\n", e)
		}
		os.Exit(1)
	}

	//directQueryInstances(pkg)
	walkAstAndJoinInstances(pkg)
}

func directQueryInstances(pkg *packages.Package) {
	// directly query Instances, and try to map it to the enclosing AST.
	for k, v := range pkg.TypesInfo.Instances {
		fmt.Println("ident", k)
		fmt.Printf("  instance: type=%v, args=%v\n", v.Type, getListOfTypes(v.TypeArgs))
	}
}

// getListOfTypes extracts a slice of types.Type from a given TypeList.
func getListOfTypes(tl *types.TypeList) []types.Type {
	var sl []types.Type
	for i := 0; i < tl.Len(); i++ {
		sl = append(sl, tl.At(i))
	}
	return sl
}

// From: https://github.com/golang/exp/tree/master/typeparams/example
//
//	We say that a type is instantiated if it is created from a generic type by
//	substituting type arguments for type parameters. Instantiation can occur via
//	explicitly provided type arguments, as in the expression T[A_1, ..., A_n],
//	or implicitly, through type inference.

func walkAstAndJoinInstances(pkg *packages.Package) {
	for _, fileAst := range pkg.Syntax {
		ast.Inspect(fileAst, func(n ast.Node) bool {
			if cexpr, ok := n.(*ast.CallExpr); ok {
				var id *ast.Ident
				switch fn := cexpr.Fun.(type) {
				case *ast.Ident:
					id = fn
				case *ast.SelectorExpr:
					id = fn.Sel
				case *ast.IndexListExpr:
					if sel, ok := fn.X.(*ast.SelectorExpr); ok {
						id = sel.Sel
					} else {
						id = fn.X.(*ast.Ident)
					}
				}

				if id != nil {
					if v, ok := pkg.TypesInfo.Instances[id]; ok {
						fmt.Println("call", id)
						fmt.Printf("  instantiation type=%v args=%v\n", v.Type, getListOfTypes(v.TypeArgs))
					}
				}
			}
			return true
		})
	}
}

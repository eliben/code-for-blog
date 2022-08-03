// Demonstrates how to display Go errors in packages loaded by XTGP.
package main

import (
	"flag"
	"fmt"
	"go/token"
	"log"

	"golang.org/x/tools/go/packages"
)

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
	if pkg.IllTyped {
		// Note: IllTyped is set only if NeedTypes is set
		fmt.Println("package or dependencies contain errors")
	}

	if len(pkg.Errors) > 0 {
		fmt.Printf("package %v has errors:\n", pkg.PkgPath)

		for _, e := range pkg.Errors {
			fmt.Println(e)
			fmt.Println(e.Kind)
		}
	}

	//	This will dump the AST regardless of errors.
	//for _, fileAst := range pkg.Syntax {
	//ast.Print(fset, fileAst)
	//}
}

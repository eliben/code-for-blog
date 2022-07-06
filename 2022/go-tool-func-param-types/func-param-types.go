package main

import (
	"flag"
	"fmt"
	"go/ast"
	"go/token"
	"go/types"
	"log"
	"strings"

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
	for _, fileAst := range pkg.Syntax {
		ast.Inspect(fileAst, func(n ast.Node) bool {
			if funcDecl, ok := n.(*ast.FuncDecl); ok {
				processFuncDecl(funcDecl, pkg.TypesInfo)
			}

			return true
		})
	}
}

func processFuncDecl(fd *ast.FuncDecl, tinfo *types.Info) {
	fmt.Println("=== Function", fd.Name)
	for _, field := range fd.Type.Params.List {
		var names []string
		for _, name := range field.Names {
			names = append(names, name.Name)
		}
		fmt.Println("param:", strings.Join(names, ", "))
		processTypeExpr(field.Type, tinfo)
		// Uncomment this for a full AST dump.
		//ast.Print(fset, field)
	}
}

func processTypeExpr(e ast.Expr, tinfo *types.Info) {
	switch tyExpr := e.(type) {
	case *ast.StarExpr:
		fmt.Println("  pointer to...")
		processTypeExpr(tyExpr.X, tinfo)
	case *ast.ArrayType:
		fmt.Println("  slice or array of...")
		processTypeExpr(tyExpr.Elt, tinfo)
	default:
		switch ty := tinfo.Types[e].Type.(type) {
		case *types.Basic:
			fmt.Println("  basic =", ty.Name())
		case *types.Named:
			fmt.Println("  name =", ty)
			uty := ty.Underlying()
			fmt.Println("  type =", ty.Underlying())
			if sty, ok := uty.(*types.Struct); ok {
				fmt.Println("  fields:")
				processStructParamType(sty)
			}
			fmt.Println("  pos =", fset.Position(ty.Obj().Pos()))
		default:
			fmt.Println("  unnamed type =", ty)
			if sty, ok := ty.(*types.Struct); ok {
				fmt.Println("  fields:")
				processStructParamType(sty)
			}
		}
	}
}

func processStructParamType(sty *types.Struct) {
	for i := 0; i < sty.NumFields(); i++ {
		field := sty.Field(i)
		fmt.Println("    ", field.Name(), field.Type())
	}
}

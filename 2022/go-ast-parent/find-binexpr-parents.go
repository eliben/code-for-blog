// Task: find whether a certain kind of a BinaryExpr node (with op '*') is
// nested inside another BinaryExpr node or not. Use several techniques for
// doing so.
// Note: just keeping one parent won't work because func calls, etc.

package main

import (
	"flag"
	"fmt"
	"go/ast"
	"go/token"
	"log"
	"os"

	"golang.org/x/tools/go/ast/astutil"
	"golang.org/x/tools/go/ast/inspector"
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
	if len(pkg.Errors) > 0 {
		for _, e := range pkg.Errors {
			fmt.Printf("Error: %s\n", e)
		}
		os.Exit(1)
	}

	discoverNodeParentsAdhoc(pkg)
	//discoverNodeParentsManualStack(pkg)
	//discoverNodeParentsPathInterval(pkg)
	//discoverNodeParentsWithStack(pkg)

}

// This "ad-hoc" approach sets state vars during traversal.
func discoverNodeParentsAdhoc(pkg *packages.Package) {
	for _, fileAst := range pkg.Syntax {
		ast.Inspect(fileAst, func(n ast.Node) bool {
			if bexpr, ok := n.(*ast.BinaryExpr); ok {
				// not bexpr itself because it will be "found" by findBinMulChild
				findBinMulChild(bexpr.X)
				findBinMulChild(bexpr.Y)
				return false
			}
			return true
		})
	}
}

func findBinMulChild(ancestor ast.Node) {
	ast.Inspect(ancestor, func(n ast.Node) bool {
		if bexpr, ok := n.(*ast.BinaryExpr); ok && bexpr.Op == token.MUL {
			fmt.Printf("found BinaryExpr(*) as a child of another binary expr: %v\n",
				fset.Position(n.Pos()))
		}
		return true
	})
}

// Keep a parents stack manually in the traversal code.
func discoverNodeParentsManualStack(pkg *packages.Package) {
	for _, fileAst := range pkg.Syntax {
		var ancestors []ast.Node
		ast.Inspect(fileAst, func(n ast.Node) bool {
			if bexpr, ok := n.(*ast.BinaryExpr); ok && bexpr.Op == token.MUL {
				// Walk the ancestor stack to find if one of them is also a BinaryExpr
				for i := len(ancestors) - 1; i >= 0; i-- {
					if _, ok := ancestors[i].(*ast.BinaryExpr); ok {
						fmt.Printf("found BinaryExpr(*) as a child of another binary expr: %v\n",
							fset.Position(n.Pos()))
						break
					}
				}
			}

			if n == nil {
				// Pop, since we're done with this node and its children.
				ancestors = ancestors[:len(ancestors)-1]
			} else {
				// Push this node on the stack, since its children will be visited
				// next.
				ancestors = append(ancestors, n)
			}
			return true
		})
	}
}

func discoverNodeParentsPathInterval(pkg *packages.Package) {
	for _, fileAst := range pkg.Syntax {
		ast.Inspect(fileAst, func(n ast.Node) bool {
			if bexpr, ok := n.(*ast.BinaryExpr); ok && bexpr.Op == token.MUL {
				path, _ := astutil.PathEnclosingInterval(fileAst, bexpr.Pos(), bexpr.End())
				for i := len(path) - 1; i >= 0; i-- {
					if _, ok := path[i].(*ast.BinaryExpr); ok && path[i] != bexpr {
						fmt.Printf("found BinaryExpr(*) as a child of another binary expr: %v\n",
							fset.Position(n.Pos()))
						break
					}
				}
			}
			return true
		})
	}
}

func discoverNodeParentsWithStack(pkg *packages.Package) {
	insp := inspector.New(pkg.Syntax)
	nodeTypes := []ast.Node{
		(*ast.BinaryExpr)(nil),
	}
	insp.WithStack(nodeTypes, func(n ast.Node, push bool, stack []ast.Node) bool {
		bexpr := n.(*ast.BinaryExpr)
		if push && bexpr.Op == token.MUL {
			for i := len(stack) - 2; i >= 0; i-- {
				if _, ok := stack[i].(*ast.BinaryExpr); ok {
					fmt.Printf("found BinaryExpr(*) as a child of another binary expr: %v\n",
						fset.Position(n.Pos()))
					break
				}
			}
		}
		return true
	})
}

package main

import (
	"bytes"
	"fmt"
	"go/ast"
	"go/format"
	"go/token"
	"go/types"
	"log"

	"golang.org/x/tools/go/analysis"
	"golang.org/x/tools/go/analysis/singlechecker"
)

var EmbedAnalysis = &analysis.Analyzer{
	Name: "embedanalysis",
	Doc:  "reports embeddings",
	Run:  run,
}

func main() {
	singlechecker.Main(EmbedAnalysis)
}

func run(pass *analysis.Pass) (interface{}, error) {
	for _, file := range pass.Files {
		ast.Inspect(file, func(n ast.Node) bool {
			if structTy, ok := n.(*ast.StructType); ok {
				findInFields(structTy.Fields, n, pass.TypesInfo, pass.Fset)
			} else if interfaceTy, ok := n.(*ast.InterfaceType); ok {
				findInFields(interfaceTy.Methods, n, pass.TypesInfo, pass.Fset)
			}

			return true
		})
	}

	return nil, nil
}

// findInFields finds embeddings in the field list fl. The field list is taken
// from either the fields of a struct or the method list of an interface.
func findInFields(fl *ast.FieldList, n ast.Node, tinfo *types.Info, fset *token.FileSet) {
	type FieldReport struct {
		Name string
		Kind string
		Type types.Type
	}
	var reps []FieldReport

	for _, field := range fl.List {
		if field.Names == nil {
			tv, ok := tinfo.Types[field.Type]
			if !ok {
				log.Fatal("not found", field.Type)
			}

			embName := fmt.Sprintf("%v", field.Type)

			_, hostIsStruct := n.(*ast.StructType)
			var kind string

			switch typ := tv.Type.Underlying().(type) {
			case *types.Struct:
				if hostIsStruct {
					kind = "struct (s@s)"
				} else {
					kind = "struct (s@i)"
				}
				reps = append(reps, FieldReport{embName, kind, typ})
			case *types.Interface:
				if hostIsStruct {
					kind = "interface (i@s)"
				} else {
					kind = "interface (i@i)"
				}
				reps = append(reps, FieldReport{embName, kind, typ})
			default:
			}
		}
	}

	if len(reps) > 0 {
		fmt.Printf("Found at %v\n%v\n", fset.Position(n.Pos()), nodeString(n, fset))

		for _, report := range reps {
			fmt.Printf("--> field '%s' is embedded %s: %s\n", report.Name, report.Kind, report.Type)
		}
		fmt.Println("")
	}
}

// nodeString formats a syntax tree in the style of gofmt.
func nodeString(n ast.Node, fset *token.FileSet) string {
	var buf bytes.Buffer
	format.Node(&buf, fset, n)
	return buf.String()
}

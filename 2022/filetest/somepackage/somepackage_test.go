package somepackage

import (
	"bytes"
	"go/format"
	"os"
	"path/filepath"
	"testing"
)

func TestFormatFiles(t *testing.T) {
	paths, err := filepath.Glob(filepath.Join("testdata", "*.input"))
	if err != nil {
		t.Fatal(err)
	}

	for _, path := range paths {
		_, filename := filepath.Split(path)
		testname := filename[:len(filename)-len(filepath.Ext(path))]

		t.Run(testname, func(t *testing.T) {
			source, err := os.ReadFile(path)
			if err != nil {
				t.Fatal("error reading source file:", err)
			}

			output, err := format.Source(source)
			if err != nil {
				t.Fatal("error formatting:", err)
			}

			goldenfile := filepath.Join("testdata", testname+".golden")
			want, err := os.ReadFile(goldenfile)
			if err != nil {
				t.Fatal("error reading golden file:", err)
			}

			if !bytes.Equal(output, want) {
				t.Errorf("\n==== got:\n%s\n==== want:\n%s\n", output, want)
			}
		})
	}
}

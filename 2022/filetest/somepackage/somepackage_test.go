package somepackage

import (
	"bytes"
	"go/format"
	"os"
	"path/filepath"
	"testing"
)

func TestFormatFiles(t *testing.T) {
	// Find the paths of all input files in the data directory.
	paths, err := filepath.Glob(filepath.Join("testdata", "*.input"))
	if err != nil {
		t.Fatal(err)
	}

	for _, path := range paths {
		_, filename := filepath.Split(path)
		testname := filename[:len(filename)-len(filepath.Ext(path))]

		// Each path turns into a test: the test name is the filename without the
		// extension.
		t.Run(testname, func(t *testing.T) {
			source, err := os.ReadFile(path)
			if err != nil {
				t.Fatal("error reading source file:", err)
			}

			// >>> This is the actual code under test.
			output, err := format.Source(source)
			if err != nil {
				t.Fatal("error formatting:", err)
			}
			// <<<

			// Each input file is expected to have a "golden output" file, with the
			// same path except the .input extension is replaced by .golden
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

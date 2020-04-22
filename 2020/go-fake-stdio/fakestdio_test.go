package fakestdio

import (
	"fmt"
	"io/ioutil"
	"os"
	"strings"
	"testing"
)

func TestFakeOut(t *testing.T) {
	var tests = []struct {
		wantOut string
	}{
		{"nope"},
		{"joe\n"},
		{"line1\nline2"},
		{"line1\nline2\n"},
		{strings.Repeat("joe ", 100)},
		{strings.Repeat("xyz\n", 300)},
	}

	for _, tt := range tests {
		testName := tt.wantOut
		if len(testName) > 30 {
			testName = testName[:30]
		}

		t.Run(testName, func(t *testing.T) {
			fs, err := New("")
			if err != nil {
				t.Fatal(err)
			}

			fmt.Print(tt.wantOut)

			b, err := fs.ReadAndRestore()
			if err != nil {
				t.Fatal(err)
			}

			if string(b) != tt.wantOut {
				t.Errorf("got %q, want %q", string(b), tt.wantOut)
			}
		})
	}
}

func TestFakeOutLarge(t *testing.T) {
	fs, err := New("")
	if err != nil {
		t.Fatal(err)
	}

	for i := 0; i < 500000; i++ {
		fmt.Print("fo00sd")
	}

	b, err := fs.ReadAndRestore()
	if err != nil {
		t.Fatal(err)
	}

	fmt.Println(len(b))
}

func TestFakeIn(t *testing.T) {
	var tests = []struct {
		wantIn string
	}{
		{"bamboleo"},
		{"x"},
		{"line1\nline2"},
		{"line1\nline2\n"},
		{"line1\nline2\nline3\n4\n5\n"},
		{strings.Repeat("joe ", 100)},
		{strings.Repeat("xyz\n", 300)},
	}
	for _, tt := range tests {
		testName := tt.wantIn
		if len(testName) > 30 {
			testName = testName[:30]
		}

		t.Run(testName, func(t *testing.T) {
			fs, err := New(tt.wantIn)
			if err != nil {
				t.Fatal(err)
			}

			b := make([]byte, 2048)
			n, err := os.Stdin.Read(b)
			if err != nil {
				t.Fatal(err)
			}

			bout, err := fs.ReadAndRestore()
			if err != nil {
				t.Fatal(err)
			}
			if len(bout) > 0 {
				t.Errorf("got bout=%v, want empty", bout)
			}

			if n != len(tt.wantIn) || string(b[:n]) != tt.wantIn {
				t.Errorf("got n=%d, b=%q; want n=%d, b=%q", n, string(b[:n]), len(tt.wantIn), tt.wantIn)
			}
		})
	}
}

func TestFakeInAndOut(t *testing.T) {
	wantIn := "bamboleo"
	fs, err := New(wantIn)
	if err != nil {
		t.Fatal(err)
	}

	wantOut := "joe\n"
	fmt.Print(wantOut)

	b := make([]byte, 1024)
	n, err := os.Stdin.Read(b)
	if err != nil {
		t.Fatal(err)
	}

	bout, err := fs.ReadAndRestore()
	if err != nil {
		t.Fatal(err)
	}
	if string(bout) != wantOut {
		t.Errorf("got %q, want %q", string(bout), wantOut)
	}

	if n != 8 || string(b[:n]) != wantIn {
		t.Errorf("got n=%d, b=%q; want n=%d, b=%q", n, string(b[:n]), 8, wantIn)
	}
}

func TestCloseStdin(t *testing.T) {
	wantIn := "marin\nnazar"
	fs, err := New(wantIn)
	if err != nil {
		t.Fatal(err)
	}
	fs.CloseStdin()

	// If we don't call CloseStdin and/or it doesn't do its job, this will block
	// forever because there's no EOF on os.Stdin
	b, err := ioutil.ReadAll(os.Stdin)

	if string(b) != wantIn {
		t.Errorf("got %q, want %q", string(b), wantIn)
	}
	fs.ReadAndRestore()
}

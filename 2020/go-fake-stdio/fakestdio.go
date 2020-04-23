// TODO: example in example_test?
// TODO: goroutine leak test?

package fakestdio

import (
	"bytes"
	"fmt"
	"io"
	"log"
	"os"
	"sync"
)

// FakeStdio can be used to fake stdin and capture stdout.
// Between creating a new FakeStdio and calling ReadAndRestore on it,
// code reading os.Stdin in the process will get the contents of stdinText
// passed to New. Output to os.Stdout will be captured and returned from
// ReadAndRestore.
type FakeStdio struct {
	origStdout   *os.File
	stdoutReader *os.File

	outBuf *bytes.Buffer
	outWg  *sync.WaitGroup

	origStdin   *os.File
	stdinWriter *os.File
}

func New(stdinText string) (*FakeStdio, error) {
	stdinReader, stdinWriter, err := os.Pipe()
	if err != nil {
		return nil, err
	}

	stdoutReader, stdoutWriter, err := os.Pipe()
	if err != nil {
		return nil, err
	}

	// Fake stdin and write input text.
	origStdin := os.Stdin
	os.Stdin = stdinReader

	_, err = stdinWriter.Write([]byte(stdinText))
	if err != nil {
		stdinWriter.Close()
		os.Stdin = origStdin
		return nil, err
	}

	// Fake stdout.
	origStdout := os.Stdout
	os.Stdout = stdoutWriter

	var wg sync.WaitGroup
	var outBuf bytes.Buffer

	// This goroutine continuously reads stdout into outBuf.
	wg.Add(1)
	go func() {
		defer wg.Done()
		for {
			n, err := outBuf.ReadFrom(stdoutReader)
			if err != nil && err != io.EOF {
				log.Println("read error", err)
				return
			}
			if n == 0 {
				return
			}
		}
	}()

	return &FakeStdio{
		origStdout:   origStdout,
		stdoutReader: stdoutReader,
		outBuf:       &outBuf,
		outWg:        &wg,
		origStdin:    origStdin,
		stdinWriter:  stdinWriter,
	}, nil
}

// CloseStdin closes the fake stdin. This may be necessary if the process has
// logic for reading stdin until EOF; otherwise such code would block forever.
func (sf *FakeStdio) CloseStdin() {
	if sf.stdinWriter != nil {
		sf.stdinWriter.Close()
		sf.stdinWriter = nil
	}
}

// ReadAndRestore collects all captured stdout and returns it; it also restores
// os.Stdin and os.Stdout to their original values.
func (sf *FakeStdio) ReadAndRestore() ([]byte, error) {
	if sf.stdoutReader == nil {
		return nil, fmt.Errorf("ReadAndRestore from closed FakeStdio")
	}

	// TODO: explain
	os.Stdout.Close()
	sf.outWg.Wait()

	os.Stdout = sf.origStdout
	os.Stdin = sf.origStdin

	if sf.stdoutReader != nil {
		sf.stdoutReader.Close()
		sf.stdoutReader = nil
	}

	if sf.stdinWriter != nil {
		sf.stdinWriter.Close()
		sf.stdinWriter = nil
	}

	return sf.outBuf.Bytes(), nil
}

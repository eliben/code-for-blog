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
// code reading os.Stdin will get the contents of stdinText passed to New.
// Output to os.Stdout will be captured and returned from ReadAndRestore.
// FakeStdio is not reusable; don't attempt to use it after calling
// ReadAndRestore, but it should be safe to create a new FakeStdio.
type FakeStdio struct {
	origStdout   *os.File
	stdoutReader *os.File

	outBuf *bytes.Buffer
	outWg  *sync.WaitGroup

	origStdin   *os.File
	stdinWriter *os.File
}

func New(stdinText string) (*FakeStdio, error) {
	// Pipe for stdin.
	//
	//                 ======
	//  w ------------->||||------> r
	// (stdinWriter)   ======      (os.Stdin)
	stdinReader, stdinWriter, err := os.Pipe()
	if err != nil {
		return nil, err
	}

	// Pipe for stdout.
	//
	//               ======
	//  w ----------->||||------> r
	// (os.Stdout)   ======      (stdoutReader)
	stdoutReader, stdoutWriter, err := os.Pipe()
	if err != nil {
		return nil, err
	}

	origStdin := os.Stdin
	os.Stdin = stdinReader

	_, err = stdinWriter.Write([]byte(stdinText))
	if err != nil {
		stdinWriter.Close()
		os.Stdin = origStdin
		return nil, err
	}

	origStdout := os.Stdout
	os.Stdout = stdoutWriter

	var wg sync.WaitGroup
	var outBuf bytes.Buffer

	// This goroutine reads stdout into outBuf in the background.
	// Access to outBuf is not protected because we read it only after this
	// goroutine exits.
	wg.Add(1)
	go func() {
		defer wg.Done()
		if _, err := io.Copy(&outBuf, stdoutReader); err != nil {
			log.Println(err)
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

	// Close the writer side of the faked stdout pipe. This signals to the
	// background goroutine it that it should exit. Then, wait for the goroutine
	// to actually exit so we can access outWg safely.
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

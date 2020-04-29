// Redirecting stdout coming from cgo.
//
// Eli Bendersky [https://eli.thegreenplace.net]
// This code is in the public domain.
package main

// #include <stdio.h>
//
// static inline void foo() {
//   fprintf(stderr, "** log\n");
//   printf("foo called\n");
// }
//
import "C"
import (
	"bytes"
	"fmt"
	"io"
	"log"
	"os"
	"syscall"
)

func main() {
	// Clone Stdout to origStdout.
	origStdout, err := syscall.Dup(syscall.Stdout)
	if err != nil {
		log.Fatal(err)
	}

	r, w, err := os.Pipe()
	if err != nil {
		log.Fatal(err)
	}

	// Clone the pipe's writer to the actual Stdout descriptor; from this point
	// on, writes to Stdout will go to w.
	if err = syscall.Dup2(int(w.Fd()), syscall.Stdout); err != nil {
		log.Fatal(err)
	}

	// Background goroutine that drains the reading end of the pipe.
	out := make(chan []byte)
	go func() {
		var b bytes.Buffer
		io.Copy(&b, r)
		out <- b.Bytes()
	}()

	// ----> The actual cgo call <----
	C.foo()

	// Cleanup
	C.fflush(nil)
	w.Close()
	syscall.Close(syscall.Stdout)

	// Rendezvous with the reading goroutine.
	b := <-out

	// Restore original Stdout.
	syscall.Dup2(origStdout, syscall.Stdout)
	syscall.Close(origStdout)

	fmt.Println("Captured:", string(b))
}

// Shared code for the prime protocol samples.
//
// Eli Bendersky [https://eli.thegreenplace.net]
// This code is in the public domain.
package server

import (
	"errors"
	"io"
	"net"
	"strconv"
	"strings"
	"time"
)

// ServePrimeProtocol serves our prime protocol to a single TCP connection.
func ServePrimeProtocol(conn net.Conn) error {
	defer conn.Close()
	buf := make([]byte, 1024)

	for {
		n, readerr := conn.Read(buf)
		if n > 0 {
			// Parse the read buffer to an i64
			num, err := strconv.ParseInt(strings.TrimSpace(string(buf[:n])), 10, 64)
			if err != nil {
				return err
			}

			response := "composite"
			if isPrime(num, true) {
				response = "prime"
			}
			if _, err := conn.Write([]byte(response + "\n")); err != nil {
				return err
			}
		}

		if readerr != nil {
			if errors.Is(readerr, io.EOF) || errors.Is(readerr, net.ErrClosed) {
				return nil
			}
			return readerr
		}
	}
}

// isPrime returns true if n is prime, false otherwise. If delay is true, it
// will sleep for n milliseconds before calculating.
func isPrime(n int64, delay bool) bool {
	if delay {
		time.Sleep(time.Duration(n) * time.Millisecond)
	}

	if n < 2 {
		return false
	}
	if n%2 == 0 {
		return n == 2
	}
	for i := int64(3); i*i <= n; i += 2 {
		if n%i == 0 {
			return false
		}
	}
	return true
}

package server

import (
	"errors"
	"io"
	"net"
)

type processingState int

const (
	waitForMsg processingState = iota
	inMsg
)

// ServeSerialProtocol serves our serial protocol to a single TCP connection.
func ServeSerialProtocol(conn net.Conn) error {
	defer conn.Close()

	if _, err := conn.Write([]byte{'*'}); err != nil {
		return err
	}

	var state processingState = waitForMsg
	buf := make([]byte, 1024)
	for {
		n, err := conn.Read(buf)
		for _, b := range buf[:n] {
			switch state {
			case waitForMsg:
				if b == '^' {
					state = inMsg
				}
			case inMsg:
				if b == '$' {
					state = waitForMsg
				} else {
					var bb byte = byte(b) + 1
					if _, err := conn.Write([]byte{bb}); err != nil {
						return err
					}
				}
			}
		}

		// Check error after processing the bytes received along with it.
		if err != nil {
			if errors.Is(err, io.EOF) || errors.Is(err, net.ErrClosed) {
				return nil
			} else {
				return err
			}
		}
	}
}

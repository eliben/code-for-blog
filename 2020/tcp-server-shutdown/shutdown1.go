// Socket server that can be shut down -- stop serving, in a graceful manner.
package shutdown1

import (
	"io"
	"log"
	"net"
	"sync"
)

type Server struct {
	listener net.Listener
	quit     chan interface{}
	wg       sync.WaitGroup
}

// NewServer creates and launches a new server listening on addr. When the
// server is returned, it's already listening for new connections.
func NewServer(addr string) *Server {
	s := &Server{
		quit: make(chan interface{}),
	}
	l, err := net.Listen("tcp", addr)
	if err != nil {
		log.Fatal(err)
	}
	s.listener = l
	go s.serve()
	return s
}

// Stop asks the server to stop and blocks until the server is actually stopped.
func (s *Server) Stop() {
	log.Println("asking the server to stop")
	close(s.quit)
	s.listener.Close()
	s.wg.Wait()
	log.Println("server stopped")
}

func (s *Server) serve() {
	s.wg.Add(1)

	defer func() {
		s.wg.Done()
	}()

	for {
		conn, err := s.listener.Accept()
		if err != nil {
			select {
			case <-s.quit:
				return
			default:
			}

			log.Println("accept error:", err)
		} else {
			go s.handleConection(conn)
		}
	}
}

func (s *Server) handleConection(conn net.Conn) {
	s.wg.Add(1)
	log.Printf("connection from %v", conn.RemoteAddr())

	defer func() {
		log.Printf("connection from %v done", conn.RemoteAddr())
		conn.Close()
		s.wg.Done()
	}()

	buf := make([]byte, 2048)
	for {
		n, err := conn.Read(buf)
		if err != nil && err != io.EOF {
			log.Println("read error", err)
			return
		}
		if n == 0 {
			return
		}
		log.Printf("received from %v: %s", conn.RemoteAddr(), string(buf[:n]))
	}
}

func init() {
	log.SetFlags(log.Ltime | log.Lmicroseconds)
}

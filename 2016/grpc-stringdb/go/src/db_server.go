// db_server is a simple string database server with a gRPC interface.
package main

import (
	"flag"
	"fmt"
	"net"
	"net/http"
	"sync"

	"golang.org/x/net/context"
	"google.golang.org/grpc"
	"google.golang.org/grpc/grpclog"

	pb "stringdb"
)

var (
	debugport = flag.Int("debugport", 44555, "Debugging port for net/trace")
	port      = flag.Int("port", 4050, "The server port")
)

type stringdbServer struct {
	mu sync.Mutex
	db map[string]string
}

func (s *stringdbServer) GetValue(ctx context.Context, r *pb.GetValueRequest) (*pb.GetValueReply, error) {
	s.mu.Lock()
	defer s.mu.Unlock()
	// Here we rely on the map's default of an empty string when no key exists.
	return &pb.GetValueReply{s.db[r.Key]}, nil
}

func (s *stringdbServer) SetValue(ctx context.Context, r *pb.SetValueRequest) (*pb.SetValueReply, error) {
	s.mu.Lock()
	defer s.mu.Unlock()
	s.db[r.Key] = r.Value
	return &pb.SetValueReply{r.Value}, nil
}

func (s *stringdbServer) CountValue(ctx context.Context, r *pb.CountValueRequest) (*pb.CountValueReply, error) {
	s.mu.Lock()
	defer s.mu.Unlock()
	retval := -1
	v, ok := s.db[r.Key]
	if ok {
		retval = len(v)
	}
	return &pb.CountValueReply{int64(retval)}, nil
}

func newServer() *stringdbServer {
	s := new(stringdbServer)
	s.db = make(map[string]string)
	return s
}

func main() {
	listenAddr := fmt.Sprintf(":%d", *port)
	lis, err := net.Listen("tcp", listenAddr)
	if err != nil {
		grpclog.Fatalf("failed to listen: %v", err)
	}

	// Register an instance of our server as a StringDbServer.
	var opts []grpc.ServerOption
	grpcServer := grpc.NewServer(opts...)
	pb.RegisterStringDbServer(grpcServer, newServer())

	// Spin up debugging HTTP server for net/trace
	debugAddr := fmt.Sprintf(":%d", *debugport)
	go http.ListenAndServe(debugAddr, nil)
	fmt.Println("Debug HTTP on", debugAddr)

	// The server listens with the connected listener.
	fmt.Println("Server listening on", *port)
	grpcServer.Serve(lis)
}

package main

import (
	"fmt"
	"net"
	pb "stringdb"
	"sync"

	"golang.org/x/net/context"
	"google.golang.org/grpc"
	"google.golang.org/grpc/grpclog"
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
	portnum := 4050
	listenAddr := fmt.Sprintf(":%d", portnum)
	lis, err := net.Listen("tcp", listenAddr)
	if err != nil {
		grpclog.Fatalf("failed to listen: %v", err)
	}
	var opts []grpc.ServerOption
	grpcServer := grpc.NewServer(opts...)
	pb.RegisterStringDbServer(grpcServer, newServer())

	fmt.Println("Server listening on", portnum)
	grpcServer.Serve(lis)
}

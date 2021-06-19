// Basic client to contact the gRPC server.
package main

import (
	"context"
	"fmt"
	"log"
	"time"

	pb "grpc-stringdb-sample/stringdb"

	"google.golang.org/grpc"
)

const (
	address = "localhost:4050"
)

func main() {
	// Set up a connection to the server.
	conn, err := grpc.Dial(address, grpc.WithInsecure(), grpc.WithBlock())
	if err != nil {
		log.Fatalf("did not connect: %v", err)
	}
	defer conn.Close()
	c := pb.NewStringDbClient(conn)

	ctx, cancel := context.WithTimeout(context.Background(), time.Second)
	defer cancel()
	_, err = c.SetValue(ctx, &pb.SetValueRequest{Key: "foo", Value: "bar"})
	if err != nil {
		log.Fatal(err)
	}
	_, err = c.SetValue(ctx, &pb.SetValueRequest{Key: "baz", Value: "anaconda"})
	if err != nil {
		log.Fatal(err)
	}

	reply, err := c.GetValue(ctx, &pb.GetValueRequest{Key: "baz"})
	if err != nil {
		log.Fatal(err)
	}

	fmt.Println(reply)
}

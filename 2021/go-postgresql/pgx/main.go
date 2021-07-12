package main

import (
	"context"
	"fmt"
	"log"
	"os"

	"github.com/jackc/pgx/v4"
)

func Check(err error) {
	if err != nil {
		log.Fatal(err)
	}
}

func main() {
	ctx := context.Background()
	conn, err := pgx.Connect(ctx, os.Getenv("MOOCDSN"))
	Check(err)
	defer conn.Close(ctx)

	var greeting string
	err = conn.QueryRow(ctx, "select 'Hello, world!'").Scan(&greeting)
	if err != nil {
		fmt.Fprintf(os.Stderr, "QueryRow failed: %v\n", err)
		os.Exit(1)
	}

	fmt.Println(greeting)
}

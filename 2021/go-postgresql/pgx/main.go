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

	users, err := dbAllUsersForCourse(ctx, conn, 2)
	Check(err)
	fmt.Println(users)
}

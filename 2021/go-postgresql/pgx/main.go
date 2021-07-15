// Interacting with a PostgreSQL database using pgx.
//
// Eli Bendersky [https://eli.thegreenplace.net]
// This code is in the public domain.
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

	courses, err := dbAllCoursesForUser(ctx, conn, 5)
	Check(err)
	fmt.Println(courses)

	projects, err := dbAllProjectsForUser(ctx, conn, 5)
	Check(err)
	fmt.Println(projects)
}

package main

import (
	"database/sql"
	"fmt"
	"log"
	"os"

	_ "github.com/lib/pq"
	// To use the pgx driver, import this instead of pq. You'll also have to
	// change the driverName param of sql.Open from "postgres" to "pgx".
	// There's no need to update db.go, since pq.Array will work just fine with
	// pgx (but it does incur importing pq).
	// See https://github.com/jackc/pgx/issues/72 for details on array usage
	//_ "github.com/jackc/pgx/v4/stdlib"
)

func Check(err error) {
	if err != nil {
		log.Fatal(err)
	}
}

func main() {
	db, err := sql.Open("postgres", os.Getenv("MOOCDSN"))
	Check(err)
	defer db.Close()

	users, err := dbAllUsersForCourse(db, 2)
	Check(err)
	fmt.Println(users)

	courses, err := dbAllCoursesForUser(db, 5)
	Check(err)
	fmt.Println(courses)

	projects, err := dbAllProjectsForUser(db, 5)
	Check(err)
	fmt.Println(projects)
}

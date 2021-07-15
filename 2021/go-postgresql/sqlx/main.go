// Interacting with a PostgreSQL database using sqlx.
//
// Eli Bendersky [https://eli.thegreenplace.net]
// This code is in the public domain.
package main

import (
	"fmt"
	"log"
	"os"

	"github.com/jmoiron/sqlx"
	_ "github.com/lib/pq"
)

func Check(err error) {
	if err != nil {
		log.Fatal(err)
	}
}

func main() {
	db, err := sqlx.Open("postgres", os.Getenv("MOOCDSN"))
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

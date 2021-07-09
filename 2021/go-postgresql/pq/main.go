package main

import (
	"database/sql"
	"fmt"
	"log"

	_ "github.com/lib/pq"
	// To use the pgx driver, import this instead of pq. You'll also have to
	// change the driverName param of sql.Open from "postgres" to "pgx".
	//_ "github.com/jackc/pgx/v4/stdlib"
)

func Check(err error) {
	if err != nil {
		log.Fatal(err)
	}
}

func main() {
	dbpath := "postgresql://testuser:testpassword@localhost/testmooc"
	db, err := sql.Open("postgres", dbpath)
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

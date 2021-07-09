package main

import (
	"database/sql"
	"fmt"
	"log"

	_ "github.com/lib/pq"
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
}

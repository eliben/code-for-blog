package main

import (
	"database/sql"
	"fmt"
	"log"

	_ "github.com/mattn/go-sqlite3"
)

func main() {
	// Open the database file in /tmp/
	db, err := sql.Open("sqlite3", "/tmp/example.db")
	if err != nil {
		log.Fatal(err)
	}
	defer db.Close()

	// Create a table if it doesn't exist
	createTableSQL := `CREATE TABLE IF NOT EXISTS users (
		id INTEGER PRIMARY KEY AUTOINCREMENT,
		name TEXT NOT NULL,
		age INTEGER NOT NULL
	);`
	_, err = db.Exec(createTableSQL)
	if err != nil {
		log.Fatal(err)
	}

	// Insert some data
	insertUserSQL := `INSERT INTO users (name, age) VALUES (?, ?)`
	_, err = db.Exec(insertUserSQL, "Alice", 30)
	if err != nil {
		log.Fatal(err)
	}
	_, err = db.Exec(insertUserSQL, "Bob", 25)
	if err != nil {
		log.Fatal(err)
	}

	// Query the data
	querySQL := `SELECT id, name, age FROM users`
	rows, err := db.Query(querySQL)
	if err != nil {
		log.Fatal(err)
	}
	defer rows.Close()

	fmt.Println("User data:")
	for rows.Next() {
		var id int
		var name string
		var age int
		err = rows.Scan(&id, &name, &age)
		if err != nil {
			log.Fatal(err)
		}
		fmt.Printf("ID: %d, Name: %s, Age: %d\n", id, name, age)
	}
	err = rows.Err()
	if err != nil {
		log.Fatal(err)
	}
}

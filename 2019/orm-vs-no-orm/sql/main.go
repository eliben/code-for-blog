// Interacting with a database using pure database/sql. Main program.
//
// Eli Bendersky [http://eli.thegreenplace.net]
// This code is in the public domain.
package main

import (
	"database/sql"
	"encoding/json"
	"fmt"
	"log"
	"time"

	_ "github.com/mattn/go-sqlite3"
)

func Check(err error) {
	if err != nil {
		log.Fatal(err)
	}
}

func CheckId(result int64, err error) int64 {
	if err != nil {
		log.Fatal(err)
	}
	return result
}

func main() {
	db, err := sql.Open("sqlite3", "blogdb.db")
	if err != nil {
		log.Fatal(err)
	}
	defer db.Close()
	Check(dbClean(db))

	for _, t := range []string{"Humor", "Programming", "Space"} {
		Check(dbAddTag(db, t))
	}

	id := CheckId(dbAddPost(db, time.Now(), "On space", "Space is big",
		[]string{"Space", "Humor"}))

	Check(dbAddComment(db, id, "Zed", time.Now(), "Nice post!!!!"))
	Check(dbAddComment(db, id, "Abby", time.Now(), "I see issues"))

	CheckId(dbAddPost(db, time.Now(), "My space journey", "Wooosh", []string{"Space"}))
	CheckId(dbAddPost(db, time.Now(), "Coding", "Code is fun", []string{"Programming"}))
	id = CheckId(dbAddPost(db, time.Now(), "Coding and humor", "Code is funny",
		[]string{"Humor", "Programming"}))

	Check(dbAddComment(db, id, "John", time.Now(), "Yeah totally"))
	Check(dbAddComment(db, id, "Abby", time.Now(), "Hahaha"))

	posts, err := dbAllPostsInTag(db, 3)
	if err != nil {
		log.Fatal(err)
	}
	b, _ := json.MarshalIndent(posts, "", "  ")
	fmt.Println(string(b))

	post, comments, tags, err := dbPostFullDetails(db, id)
	if err != nil {
		log.Fatal(err)
	}
	fmt.Println(post)
	fmt.Println(comments)
	fmt.Println(tags)
}

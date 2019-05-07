// Interacting with a database using gorm.
//
// Eli Bendersky [http://eli.thegreenplace.net]
// This code is in the public domain.
package main

import (
	"encoding/json"
	"flag"
	"fmt"
	"log"
	"time"

	"github.com/jinzhu/gorm"
	_ "github.com/jinzhu/gorm/dialects/sqlite"
)

type Post struct {
	gorm.Model
	Published time.Time
	Title     string
	Content   string
	Comments  []Comment `gorm:"foreignkey:PostID"`
	Tags      []*Tag    `gorm:"many2many:post_tags;"`
}

type Tag struct {
	gorm.Model
	Name  string
	Posts []*Post `gorm:"many2many:post_tags;"`
}

type Comment struct {
	gorm.Model
	Author    string
	Published time.Time
	Content   string
	PostID    int64
}

func allPostsInTag(db *gorm.DB, t *Tag) ([]Post, error) {
	var posts []Post
	r := db.Model(t).Related(&posts, "Posts")
	if r.Error != nil {
		return nil, r.Error
	}
	return posts, nil
}

func main() {
	logSql := flag.Bool("logsql", false, "log SQL invocations from Gorm")
	flag.Parse()
	db, err := gorm.Open("sqlite3", "blogdb.db")
	if err != nil {
		log.Fatal(err)
	}
	defer db.Close()

	if *logSql {
		// Enable gorm logging to show SQL
		db.LogMode(true)
	}

	// Migrate the schema
	if r := db.AutoMigrate(&Post{}, &Tag{}, &Comment{}); r.Error != nil {
		log.Fatal(r.Error)
	}

	// Clean up DB
	db.Unscoped().Delete(&Post{})
	db.Unscoped().Delete(&Tag{})
	db.Unscoped().Delete(&Comment{})

	// Create tags
	humorTag := Tag{Name: "Humor"}
	if r := db.Create(&humorTag); r.Error != nil {
		log.Fatal(r.Error)
	}
	programmingTag := Tag{Name: "Programming"}
	if r := db.Create(&programmingTag); r.Error != nil {
		log.Fatal(r.Error)
	}
	spaceTag := Tag{Name: "Space"}
	if r := db.Create(&spaceTag); r.Error != nil {
		log.Fatal(r.Error)
	}

	// Create post with tag
	r := db.Create(&Post{Published: time.Now(), Title: "My space journey",
		Content: "Wooosh", Comments: nil, Tags: []*Tag{&spaceTag}})
	if r.Error != nil {
		log.Fatal(r.Error)
	}

	// Create post with tag and comments
	r = db.Create(&Post{Published: time.Now(), Title: "On space",
		Content: "Space is big",
		Comments: []Comment{
			Comment{Author: "Zed", Published: time.Now(), Content: "Nice post!!!!"},
			Comment{Author: "Abby", Published: time.Now(), Content: "I see issues"}},
		Tags: []*Tag{&spaceTag, &humorTag}})
	if r.Error != nil {
		log.Fatal(r.Error)
	}

	// Another post with tag and comments
	r = db.Create(&Post{Published: time.Now(), Title: "Coding and humor",
		Content: "Code is funny",
		Comments: []Comment{
			Comment{Author: "John", Published: time.Now(), Content: "Yeah totally"},
			Comment{Author: "Abby", Published: time.Now(), Content: "Hahaha"}},
		Tags: []*Tag{&humorTag, &programmingTag}})
	if r.Error != nil {
		log.Fatal(r.Error)
	}

	// Query all posts for a tag
	posts, err := allPostsInTag(db, &spaceTag)
	if err != nil {
		log.Fatal(err)
	}
	b, _ := json.MarshalIndent(posts, "", "  ")
	fmt.Println(string(b))

	// Find all details for a given post (by title)
	var post Post
	r = db.Where("title = ?", "On space").First(&post)
	if r.Error != nil {
		log.Fatal(r.Error)
	}
	fmt.Println("Post 'On space'=", post)

	// ... all its comments
	var comments []Comment
	r = db.Model(&post).Related(&comments)
	if r.Error != nil {
		log.Fatal(r.Error)
	}
	fmt.Println(comments)

	// ... all its tags
	var tags []Tag
	r = db.Model(&post).Related(&tags, "Tags")
	if r.Error != nil {
		log.Fatal(r.Error)
	}
	fmt.Println(tags)
}

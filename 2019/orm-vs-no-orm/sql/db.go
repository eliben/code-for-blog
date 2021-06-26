// Interacting with a database using pure database/sql. Functions encapsulating
// DB interface.
//
// Eli Bendersky [http://eli.thegreenplace.net]
// This code is in the public domain.
package main

import (
	"database/sql"
	"fmt"
	"time"
)

type post struct {
	Id        int64
	Published time.Time
	Title     string
	Content   string
}

type tag struct {
	Id   int64
	Name string
}

type comment struct {
	Id        int64
	Author    string
	Published time.Time
	Content   string
}

func dbClean(db *sql.DB) error {
	for _, table := range []string{"Tag", "Post", "PostTag", "Comment"} {
		// Unsafe SQL string interpolation, but ? doesn't work with 'delete from'
		// and this function is only used for internal testing.
		_, err := db.Exec(fmt.Sprintf("delete from %s", table))
		if err != nil {
			return err
		}
	}
	return nil
}

func dbAddTag(db *sql.DB, tagName string) error {
	_, err := db.Exec("insert into Tag(name) values(?)", tagName)
	return err
}

func dbAddPost(db *sql.DB, published time.Time, title string, content string, tags []string) (int64, error) {
	r, err := db.Exec("insert into Post(published, title, content) values(?, ?, ?)",
		published, title, content)
	if err != nil {
		return 0, err
	}
	postID, err := r.LastInsertId()
	if err != nil {
		return 0, err
	}

	// For each tag requested for this post, find the tag's ID in the DB and
	// create an entry in the PostTag linking table.
	for _, tag := range tags {
		row := db.QueryRow("select tagID from Tag where name = ?", tag)
		var tagID int
		err := row.Scan(&tagID)
		if err != nil {
			return 0, err
		}

		_, err = db.Exec("insert into PostTag(postID, tagID) values(?, ?)", postID, tagID)
		if err != nil {
			return 0, err
		}
	}

	return postID, nil
}

func dbAddComment(db *sql.DB, postID int64, author string, published time.Time, content string) error {
	_, err := db.Exec("insert into Comment(postID, author, published, content) values(?, ?, ?, ?)",
		postID, author, published, content)
	if err != nil {
		return err
	}
	return nil
}

func dbAllPostsInTag(db *sql.DB, tagID int64) ([]post, error) {
	rows, err := db.Query(`
	  select Post.postID, Post.published, Post.title, Post.content
	  from Post
	  inner join PostTag on Post.postID = PostTag.postID
		where PostTag.tagID = ?`, tagID)
	if err != nil {
		return nil, err
	}
	var posts []post
	for rows.Next() {
		var p post
		err = rows.Scan(&p.Id, &p.Published, &p.Title, &p.Content)
		if err != nil {
			return nil, err
		}
		posts = append(posts, p)
	}
	return posts, nil
}

// dbPostFullDetails takes a post ID and returns the post, all comments attached
// to it, and all tags.
func dbPostFullDetails(db *sql.DB, postID int64) (post, []comment, []tag, error) {
	// Get full post entry from the Post table
	postRow := db.QueryRow(`select Post.published, Post.title, Post.content
				  								 from Post where Post.postID = ?`, postID)
	var p post
	p.Id = postID
	err := postRow.Scan(&p.Published, &p.Title, &p.Content)
	if err != nil {
		return post{}, nil, nil, err

	}

	// Get all comments for the post from Comment
	commRows, err := db.Query(`select Comment.commentID, Comment.author,
	                                  Comment.published, Comment.content
	                            from Comment where Comment.postID = ?`, postID)
	if err != nil {
		return post{}, nil, nil, err
	}
	var comments []comment
	for commRows.Next() {
		var c comment
		err = commRows.Scan(&c.Id, &c.Author, &c.Published, &c.Content)
		if err != nil {
			return post{}, nil, nil, err
		}
		comments = append(comments, c)
	}

	// Get all tags for the post from PostTag and Tag
	var tags []tag
	tagsRows, err := db.Query(`select Tag.tagID, Tag.name
	                           from PostTag
	                            inner join Tag on Tag.tagID = PostTag.tagID
	                            where PostTag.postID = ?`, postID)
	if err != nil {
		return post{}, nil, nil, err
	}
	for tagsRows.Next() {
		var t tag
		err = tagsRows.Scan(&t.Id, &t.Name)
		if err != nil {
			return post{}, nil, nil, err
		}
		tags = append(tags, t)
	}
	return p, comments, tags, nil
}

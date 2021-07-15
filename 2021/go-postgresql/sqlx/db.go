// Interacting with a PostgreSQL database using sqlx.
//
// Eli Bendersky [https://eli.thegreenplace.net]
// This code is in the public domain.
package main

import (
	"time"

	"github.com/jmoiron/sqlx"
	"github.com/lib/pq"
)

type course struct {
	Id        int64
	CreatedAt time.Time `db:"created_at"`
	Title     string
	Hashtags  pq.StringArray
}

type user struct {
	Id   int64
	Name string
}

type project struct {
	Id      int64
	Name    string
	Content string
}

func dbAllUsersForCourse(db *sqlx.DB, courseId int64) ([]user, error) {
	var users []user
	err := db.Select(&users, `
		select users.id, users.name
		from users
		inner join course_user on users.id = course_user.user_id
		where course_user.course_id = $1`, courseId)
	if err != nil {
		return nil, err
	}
	return users, nil
}

func dbAllCoursesForUser(db *sqlx.DB, userId int64) ([]course, error) {
	var courses []course
	err := db.Select(&courses, `
		select courses.id, courses.created_at, courses.title, courses.hashtags
		from courses
		inner join course_user on courses.id = course_user.course_id
		where course_user.user_id = $1`, userId)
	if err != nil {
		return nil, err
	}
	return courses, nil
}

func dbAllProjectsForUser(db *sqlx.DB, userId int64) ([]project, error) {
	var projects []project
	err := db.Select(&projects, `
		select projects.id, projects.name, projects.content
		from courses
		inner join course_user on courses.id = course_user.course_id
		inner join projects on courses.id = projects.course_id
		where course_user.user_id = $1`, userId)
	if err != nil {
		return nil, err
	}
	return projects, nil
}

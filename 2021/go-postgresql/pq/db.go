package main

import (
	"database/sql"
	"time"
)

type course struct {
	Id        int64
	CreatedAt time.Time
	Title     string
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

func dbAllUsersForCourse(db *sql.DB, courseId int64) ([]user, error) {
	rows, err := db.Query(`
		select users.id, users.name
		from users
		inner join course_user on users.id = course_user.user_id
		where course_user.course_id = $1`, courseId)
	if err != nil {
		return nil, err
	}
	var users []user
	for rows.Next() {
		var u user
		err = rows.Scan(&u.Id, &u.Name)
		if err != nil {
			return nil, err
		}
		users = append(users, u)
	}
	return users, nil
}

func dbAllCoursesForUser(db *sql.DB, userId int64) ([]course, error) {
	rows, err := db.Query(`
		select courses.id, courses.created_at, courses.title
		from courses
		inner join course_user on courses.id = course_user.course_id
		where course_user.user_id = $1`, userId)
	if err != nil {
		return nil, err
	}
	var courses []course
	for rows.Next() {
		var c course
		err = rows.Scan(&c.Id, &c.CreatedAt, &c.Title)
		if err != nil {
			return nil, err
		}
		courses = append(courses, c)
	}
	return courses, nil
}

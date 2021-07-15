// Interacting with a PostgreSQL database using scany.
//
// Eli Bendersky [https://eli.thegreenplace.net]
// This code is in the public domain.
package main

import (
	"context"
	"time"

	"github.com/georgysavva/scany/pgxscan"
	"github.com/jackc/pgx/v4"
)

type course struct {
	Id        int64
	CreatedAt time.Time
	Title     string
	Hashtags  []string
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

func dbAllUsersForCourse(ctx context.Context, conn *pgx.Conn, courseId int64) ([]user, error) {
	var users []user
	err := pgxscan.Select(ctx, conn, &users, `
		select users.id, users.name
		from users
		inner join course_user on users.id = course_user.user_id
		where course_user.course_id = $1`, courseId)
	if err != nil {
		return nil, err
	}
	return users, nil
}

func dbAllCoursesForUser(ctx context.Context, conn *pgx.Conn, userId int64) ([]course, error) {
	var courses []course
	err := pgxscan.Select(ctx, conn, &courses, `
		select courses.id, courses.created_at, courses.title, courses.hashtags
		from courses
		inner join course_user on courses.id = course_user.course_id
		where course_user.user_id = $1`, userId)
	if err != nil {
		return nil, err
	}
	return courses, nil
}

func dbAllProjectsForUser(ctx context.Context, conn *pgx.Conn, userId int64) ([]project, error) {
	var projects []project
	err := pgxscan.Select(ctx, conn, &projects, `
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

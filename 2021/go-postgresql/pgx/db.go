package main

import (
	"context"
	"time"

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
	rows, err := conn.Query(ctx, `
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

func dbAllCoursesForUser(ctx context.Context, conn *pgx.Conn, userId int64) ([]course, error) {
	rows, err := conn.Query(ctx, `
		select courses.id, courses.created_at, courses.title, courses.hashtags
		from courses
		inner join course_user on courses.id = course_user.course_id
		where course_user.user_id = $1`, userId)
	if err != nil {
		return nil, err
	}
	var courses []course
	for rows.Next() {
		var c course
		err = rows.Scan(&c.Id, &c.CreatedAt, &c.Title, &c.Hashtags)
		if err != nil {
			return nil, err
		}
		courses = append(courses, c)
	}
	return courses, nil
}

func dbAllProjectsForUser(ctx context.Context, conn *pgx.Conn, userId int64) ([]project, error) {
	rows, err := conn.Query(ctx, `
		select projects.id, projects.name, projects.content
		from courses
		inner join course_user on courses.id = course_user.course_id
		inner join projects on courses.id = projects.course_id
		where course_user.user_id = $1`, userId)
	if err != nil {
		return nil, err
	}
	var projects []project
	for rows.Next() {
		var p project
		err = rows.Scan(&p.Id, &p.Name, &p.Content)
		if err != nil {
			return nil, err
		}
		projects = append(projects, p)
	}
	return projects, nil
}

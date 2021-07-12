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

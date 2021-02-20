package main

import (
	"os"

	"example.com/internal/task"
	"github.com/labstack/echo/v4"
	"github.com/labstack/echo/v4/middleware"
)

func main() {
	// Echo instance
	e := echo.New()

	// Middleware
	e.Use(middleware.Logger())
	e.Use(middleware.Recover())
	e.Pre(middleware.AddTrailingSlash())

	taskserver := task.NewTaskServer()
	task.RegisterHandlers(e, taskserver)

	// Start server
	e.Logger.Fatal(e.Start("localhost:" + os.Getenv("SERVERPORT")))
}

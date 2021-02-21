// Sample task server using oapi-codegen boilerplate.
//
// Eli Bendersky [https://eli.thegreenplace.net]
// This code is in the public domain.
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
	e.Pre(middleware.RemoveTrailingSlash())
	e.Use(middleware.Logger())

	taskserver := task.NewTaskServer()
	task.RegisterHandlers(e, taskserver)

	// Start server
	e.Logger.Fatal(e.Start("localhost:" + os.Getenv("SERVERPORT")))
}

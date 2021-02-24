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
	// Set up echo server/router and middleware.
	// The paths in out OpenAPI spec are defined w/o trailing slashes, but we want
	// to accept requests *with* trailing slashes too - so use the
	// RemoveTrailingSlash middleware.
	e := echo.New()
	e.Pre(middleware.RemoveTrailingSlash())
	e.Use(middleware.Logger())

	taskserver := task.NewTaskServer()
	task.RegisterHandlers(e, taskserver)

	e.Logger.Fatal(e.Start("localhost:" + os.Getenv("SERVERPORT")))
}

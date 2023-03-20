package plugin

import "example.com/content"

type Htmlizer interface {
	Hooks() []string
	ProcessRole(role string, val string, post content.Post) string
	ProcessContents(val string, post content.Post) string
}

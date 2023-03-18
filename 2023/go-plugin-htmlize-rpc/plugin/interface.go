package plugin

import "example.com/content"

type RoleHook interface {
	ProcessRole(role string, val string, post content.Post) string
}

type ContentsHook interface {
	ProcessContents(val string, post content.Post) string
}

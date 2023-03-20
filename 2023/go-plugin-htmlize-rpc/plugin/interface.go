package plugin

import "example.com/content"

// Htmlizer is the interface plugins have to implement. To avoid calling the
// plugin for roles it doesn't support, it has to tell the plugin managers
// which roles it wants to be invoked on by implementing the Hooks() method.
type Htmlizer interface {
	// Hooks returns a list of the hooks this plugin wants to register.
	// Hooks can have one of the following forms:
	//
	//  * "contents": the plugin's ProcessContents method will be called on
	//                the post's complete contents.
	//
	// * "role:NN": the plugin's ProcessRole method will be called with role=NN
	//              and the role's value when a :NN: role is encountered in the
	//              input.
	Hooks() []string

	// ProcessRole is called on roles the plugin requested in the list returned
	// by Hooks(). It takes the role name, role value in the input and the post
	// and should return the transformed role value.
	ProcessRole(role string, val string, post content.Post) string

	// ProcessContents is called on the entire post contents, if requested in
	// Hooks(). It takes the contents and the post and should return the
	// transformed contents.
	ProcessContents(val string, post content.Post) string
}

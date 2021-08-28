An example of run-time plugins in Go.

The `plugins` directory contains the code for the plugins (each one has its
own directory and a file in `package main`); the rest of the code is the
`htmlize` application and the shared code.

To build the plugins, run `./buildplugins.sh`. This should create two `.so`
files in the current directory. Then, run the application with:

		$ go run . -plugindir=. < sampletext.txt

## How it works

The main interface between the application and plugins is in
`plugin/manager.go`, which contains the types of functions the plugins are
expected to register as hooks, and the `PluginManager` type used by the plugins
to set up hooks, while the application uses it to apply the hooks when
appropriate.

`plugin/loader.go` implements the plugin loading logic in the application. Each
plugin is expected to expose an `InitPlugin` function, which gets passed a
`PluginManager` it can then use to register hooks.

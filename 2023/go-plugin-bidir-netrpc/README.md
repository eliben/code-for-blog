Example of a plugin using `hashicorp/go-plugin` that demonstrates bidirectional
communication between the plugin and main application using net/rpc.

This plugin is similar to the `bidirectional/plugin-go-grpc` example in the
`go-plugin` repository, but it's using net/rpc instead of gRPC.

Usage
-----

* Run `./build.sh` to build the plugin and main application.
* Run `./basic-bidir` to run the application (it invokes the plugin)

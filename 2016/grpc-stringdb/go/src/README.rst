This is a Go server for the "string database" gRPC sample.

The server works similarly to the C++ server, listening on port 4050. To run it,
gRPC has to be installed for Go following the instructions from
https://github.com/grpc/grpc-go.

Also, since this sample lives in its own "Go workspace", ``GOPATH`` has to be
defined as follows:

.. sourcecode:: text

	# Assuming we run this from the grpc-stringdb/go/ directory
	$ GOPATH=$PWD:$GOPATH go run src/db_server.go

Note: in the above, ``$GOPATH`` is your usual path where Go stuff is installed,
including gRPC. If you install everything in the same workspace you may not need
it.

The compiled ``.pb.go`` comes with the repository. To rebuild it from the
``.proto`` file, run ``protoc`` with the ``grpc`` plugin as follows, from the
grpc/go/ directory:

.. sourcecode:: text

	$ PATH=$PATH:$GOPATH/bin protoc --go_out=plugins=grpc:src/stringdb -I../ ../stringdb.proto

The ``PATH`` setting is required for ``protoc`` to find the grpc plugin.

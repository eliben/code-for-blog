This is a Go server for the "string database" gRPC sample.

The server works similarly to the C++ server, listening on port 4050. To run it,
gRPC has to be installed for Go following the instructions from
https://github.com/grpc/grpc-go.

The compiled ``.pb.go`` files come with the repository. To rebuild them from the
``.proto`` file, run ``protoc`` (after following the installation instructions
to install ``protoc`` and the Go gRPC stuff), from the ``grpc/go/src``
directory:

.. sourcecode:: text

  $ protoc --go_out=stringdb --go-grpc_out=stringdb \
      --go_opt=paths=source_relative --go-grpc_opt=paths=source_relative \
      -I../../ ../../stringdb.proto

When the server runs successfully with ``go run .`` from this directory, it
reports the port it's listening on.

In a separate terminal, run the client from ``cmd/sample-client/client.go``.
It should successfully connect to the local server and send some RPCs.

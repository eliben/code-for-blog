This is a sample of using gRPC in multiple languages to implement a simple
"string database" server with clients that can access it.

It reproduces a sample I put online for achieving the same with boost.asio and
Protocol Buffers `in 2011
<https://github.com/eliben/code-for-blog/tree/master/2011/asio_protobuf_sample>`_.

After installing gRPC following the instructions on www.grpc.io, run the
`Makefile` to build the sample.

Then, in one terminal run:

.. sourcecode:: text

    $ ./db_server
    Server listening on 0.0.0.0:4050

And in another run the ``tester_client.py`` client. Make sure you have the
Python gRPC packages installed in the environment when it's run.

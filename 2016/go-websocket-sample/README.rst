go-websocket-sample
===================

To see the sample in action, run:

.. sourcecode:: text

    $ go run <path/to/server.go>

This starts up the server; it immediately reports which port it's listening on
(the port can be changed with the ``-port`` flag).

Then open a browser and visit ``http://localhost:<portnum>`` to see the HTML
page that talks with the server using websockets. The server also spins up
a `net/trace <https://godoc.org/golang.org/x/net/trace>`__ debugging page on
``/debug/requests``.

What the sample does
====================

The JavaScript code powering the served web page records mouse movement events
over the shown box and sends them to the server through a websocket. The server
echoes the data back and this is used to update the text reporting the
coordinates.

Moreover, the server also periodically sends the current time over another
websocket, which the client uses to update the time ticker at the top of the
page.

For more details just check out the source - it's very simple.

License
=======

This code is in the public domain. See the ``LICENSE`` file for more details.

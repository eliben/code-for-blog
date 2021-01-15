To test a server, run it in a separate terminal. Use the `SERVERPORT` env var
for the port.

Then run `go test -count=1 -v ./...` inside this `testing` directory, with
the same `SERVERPORT` set.

Simple manual tests with `curl` can be run with `manual.sh`.

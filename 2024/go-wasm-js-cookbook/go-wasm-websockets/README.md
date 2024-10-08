Using WebSockets (WS) from Go via WebAssembly.

This is a Go server that serves a page and talks to it via WS. Run
`make build`, followed by `make serve` and visit `http://localhost:4050`.

The client side is implemented entirely in Go (see `wasmsrc/wsclient.go`),
with a minimal HTML structure in `index.html`



To test a prime server, run:

    python3 server-test <path-to-server>

To run the servers that rely on Redis caching, first Redis should be installed
locally and `redis-server` should be set to run in the background. Next, use
`npm` to install the `redis` package.

The `redis` package changed since this post was originally written; in 2026
I've adjusted it to work with existing code via the legacy client mode.

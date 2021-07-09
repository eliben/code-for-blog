Make sure the [migrate tool](https://github.com/golang-migrate/migrate) is
installed.

Create a `testmooc` database in postgresql, with user `testuser` and password
`testpassword`.

Then run:

    $ migrate -database postgres://testuser:testpassword@localhost/testmooc -path . up

To create new steps, run:

    $ migrate create -ext sql -dir . -seq <name>

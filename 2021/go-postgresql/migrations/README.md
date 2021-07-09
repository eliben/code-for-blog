Create a `testmooc` database in postgresql, with user `testuser` and password
`testpassword`.

Then run:

    $ migrate -database postgres://testuser:testpassword@localhost/testmooc -path . up


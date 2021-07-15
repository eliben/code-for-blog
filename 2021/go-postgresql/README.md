# Samples for PostgreSQL access with Go

## Setting up a database

If you have PostgreSQL running locally, set up a new database. For example, it
could be named `testmooc`. You'll also need a username/password pair that works
with your local DB instance. Here's an example of setting it up (assuming you
have the default `postgres` user set up):

    $ sudo -u postgres psql

Now we can create a new DB in the psql shell:

    # create database testmooc;

And a new role (user) with password:

    # create role testuser with login password 'testpassword'; 

If everything was set up correctly, you should now be able to access this DB
locally with `psql`:

    $ psql postgres://testuser:testpassword@localhost/testmooc

## Populating the database

I've been using the [migrate tool](https://github.com/golang-migrate/migrate).

From the `migrations` directory, run:

    $ migrate -database postgres://testuser:testpassword@localhost/testmooc -path . up

To create new migration steps, run:

    $ migrate create -ext sql -dir . -seq <name>

If you don't want to use `migrate`, you can pipe the SQL files into `psql` in
the right order instead.

## Accessing the database from Go

With this setup out of the way, you should be able to run the samples. The
samples assume the DSN of the database is provided in the `MOOCDSN` env var.
From any sub-directory (except `migrations`), run:

    $ MOOCDSN=postgres://testuser:testpassword@localhost/testmooc go run .

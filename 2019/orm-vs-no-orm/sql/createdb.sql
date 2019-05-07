-- SQL script for creating the database tables in SQLite.
-- Use it by piping into sqlite3 with a new DB name:
--
--  $ sqlite3 somedb.db < createdb.sql
--
-- Eli Bendersky [http://eli.thegreenplace.net]
-- This code is in the public domain.


-- Foreign key constraints have to be enabled explicitly in SQLite.
PRAGMA foreign_keys = ON;

create table Post (
    postID integer primary key,
    published date,
    title text,
    content text
);

create table Comment (
    commentID integer primary key,
    postID integer,
    author text,
    published date,
    content text,

    foreign key(postID) references Post(postID)
);

create table Tag (
    tagID integer primary key,
    name text unique
);

-- Linking table for the many-to-many relationship between Tag and Post
create table PostTag (
    postID integer,
    tagID integer,

    foreign key(postID) references Post(postID),
    foreign key(tagID) references Tag(tagID)
);

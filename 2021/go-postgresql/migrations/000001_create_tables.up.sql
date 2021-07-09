create table if not exists courses (
    id bigserial primary key,
    created_at timestamp(0) with time zone not null default now(),
    title text not null,
    hashtags text[]
);

create table if not exists projects (
    id bigserial primary key,
    name text not null,
    content text not null,
    course_id bigint not null references courses (id) on delete cascade
);

create table if not exists users (
    id bigserial primary key,
    name text not null
);

create table if not exists course_user (
    course_id bigint not null references courses (id) on delete cascade,
    user_id bigint not null references users (id) on delete cascade,
    constraint course_user_key primary key (course_id, user_id)
);

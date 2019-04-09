-- Companion SQL for the blog post about SQL joins.
-- Example like joins.sql but joining on multiple columns.
--
-- Eli Bendersky [http://eli.thegreenplace.net]
-- This code is in the public domain.
drop table if exists t1;
drop table if exists t2;

create table t1 (
    id int primary key,
    name text,
    ranking int);

create table t2 (
    code char(1) primary key,
    id int,
    ranking int);

insert into t1 values(1, 'Joanne', 7);
insert into t1 values(2, 'Sam', 7);
insert into t1 values(3, 'Emmanuel', 6);
insert into t1 values(4, 'Brayden', 2);

insert into t2 values('x', 2, 8);
insert into t2 values('z', 3, 6);

select * from t1;
select * from t2;

select * from t1 inner join t2 on t1.id = t2.id and t1.ranking = t2.ranking;
select * from t1 inner join t2 using (id, ranking);
select * from t1 natural inner join t2;


select * from t1 left outer join t2 using (id, ranking);

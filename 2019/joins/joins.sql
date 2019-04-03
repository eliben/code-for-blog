-- Note: use with Postgres (tested with version 9.5)

-- Re-creates two tables, regardless if they were in the DB already.
drop table if exists t1;
drop table if exists t2;

create table t1 (
    id int primary key,
    name text);

create table t2 (
    code char(1) primary key,
    id int);

insert into t1 values(1, 'Joanne');
insert into t1 values(2, 'Sam');
insert into t1 values(3, 'Emmanuel');
insert into t1 values(4, 'Brayden');

insert into t2 values('x', 2);
insert into t2 values('z', 3);
insert into t2 values('a', 7);

-- Cross join, both syntaxes
select * from t1, t2;
select * from t1 cross join t2;

-- Inner join, with and w/o the "inner"
select * from t1 inner join t2 on t1.id = t2.id;
select * from t1 join t2 on t1.id = t2.id;

-- Same, with "where"
select * from t1, t2 where t1.id = t2.id;

-- Same, with "using" -- common column not duplicated
select * from t1 inner join t2 using (id);

-- Same, with natural join -- common column not duplicated
select * from t1 natural inner join t2;

-- Left outer join
select * from t1 left outer join t2 using (id);

-- Equivalent using union and inner join
select id, name, code
    from t1 inner join t2 using (id)
    union
    select id, name, null
        from t1
        where id not in (select id from t2);

-- Right outer join
select * from t1 right outer join t2 using (id);

-- Full outer join
select * from t1 full outer join t2 using (id);

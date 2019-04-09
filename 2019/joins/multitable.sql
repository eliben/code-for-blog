-- Companion SQL for the blog post about SQL joins.
-- Joining multiple tables.
--
-- Eli Bendersky [http://eli.thegreenplace.net]
-- This code is in the public domain.

-- Have to drop with 'cascade' now because of foreign key constraints.
drop table if exists customers cascade;
drop table if exists items cascade;
drop table if exists orders cascade;

create table customers (
    customerid int primary key,
    name text);

create table items (
    itemid int primary key,
    description text,
    price real);

create table orders (
    customerid int references customers(customerid),
    itemid int references items(itemid),
    orderdate date);

insert into customers values(1, 'Robert');
insert into customers values(2, 'Jennifer');
insert into customers values(3, 'Yoshi');
insert into customers values(4, 'Xi');

insert into items values(1, 'Napkins', 1.5);
insert into items values(2, 'Granola', 4.25);
insert into items values(3, 'Cheese', 3.0);

insert into orders values(1, 2, '2019-03-02');
insert into orders values(1, 3, '2019-03-02');
insert into orders values(1, 1, '2019-03-03');
insert into orders values(2, 1, '2019-02-22');
insert into orders values(3, 3, '2019-01-15');
insert into orders values(3, 2, '2019-02-20');
insert into orders values(4, 3, '2019-02-21');
insert into orders values(4, 3, '2019-02-22');

select * from customers;
select * from items;
select * from orders;

select name, orderdate, description
    from (customers
    inner join orders using (customerid))
    inner join items using (itemid)
    where items.description = 'Cheese';

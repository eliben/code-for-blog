-- Creating IDs manually here so it's easy to populate the course_user table.

insert into courses values
  (1, '2018-01-04'::timestamp, 'Introduction to Biology'),
  (2, '2020-11-12'::timestamp, 'Python 101'),
  (3, '2019-05-28'::timestamp, 'Organic Chemistry'),
  (4, '2012-02-29'::timestamp, 'French cuisine'),
  (5, '2016-06-16'::timestamp, 'Signals and Systems');

insert into projects values
  (1, 'Basics of dissection', '<redacted>', 1),
  (2, 'Intro to dicts', 'lots of stuff', 2),
  (3, 'Memoization', 'lispy', 2),
  (4, 'Omelette', 'yummy', 4),
  (5, 'Croissants', 'yummy^2', 4),
  (6, 'Fourier transforms', 'siny and cosy', 5),
  (7, 'Gibbs effect', 'ring-a-ding', 5),
  (8, 'Superposition', 'ax+by', 5),
  (9, 'Convolutions', 'not the ML kind', 5);

insert into users values
  (1, 'John Doe'),
  (2, 'Selena Hernandez'),
  (3, 'Hare Krishna'),
  (4, 'Manuel Lopez'),
  (5, 'Deidre Esteban'),
  (6, 'Xe Leong'),
  (7, 'Alice Mu');

insert into course_user values
  (1, 3),
  (1, 4),
  (2, 7),
  (2, 1),
  (3, 6),
  (3, 5),
  (3, 4),
  (5, 5);

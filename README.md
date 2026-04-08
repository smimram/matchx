# Stable matching for X students in 3A

This program is essentially an implementation of the [Gale-Shapely algorithm](https://en.wikipedia.org/wiki/Gale%E2%80%93Shapley_algorithm) for assigning X students to courses in 3 year, trying to adhere to students preferences while respecting numerus clausus for courses.

## Design choices

- Mandatory courses: students choosing a mandatory course are always preferred.
- Courses overlap: when a student has two courses which overlap we drop the one of greatest block number (because is has a tendency to be more optional), unless it has no remaining choice.
- courses taken twice: the case of students taking the same course twice (in two different blocks) is handled by the course overlap above.

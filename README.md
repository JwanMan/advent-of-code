# Advent of code 2024

These are my solutions for Advent of Code in 2024.  This year I'm using Guile
Scheme as the programming language.  To launch the program for a given day, run

```sh
./main <<ID>> [<<FILE>>]
```

where `<<ID>>` is the day number followed by `a` for the first part or `b` for
the second part.  If the file is not specified, it defaults to the number of the
day with two digits and extension `.in`, i.e. `01.in`.  To read from standard
input, use `-` as the file name.

Example:

```sh
./main 05b -
```

Some of the first days are written in Lua rather than Guile Scheme.  To run
them, simply replace `main` with `main.lua`.

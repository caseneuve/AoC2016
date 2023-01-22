# Advent of Code 2016 solutions

## Configuration

Create `.env` file with contents:

```
AOC_SESSION=XXXXX
YEAR=2016
```

Where `AOC_SESSION` is the session cookie (to be found in request
headers for e.g. puzzle input page when logged into the AoC account).

`YEAR` is optional -- if not specified, current year will be used.

All input should go to common directory `input/`, each day having a format `dayNN`.

## Clojure (via babashka)

Configuration is stored in `bb.edn`.

To install dependencies, just run `bb` command.

To see available tasks, run `bb tasks`.

```
$ bb tasks
The following tasks are available:

template   create template for solution [day and year default to current]
fetch      get input for given day [requires session token stored in `.env` file ; args default to today]
boot       fetch input data and create template for given date
solve      run solution for given day [day defaults to today]
```

## Solutions

Day | Clojure 
---:|------------------------- 
 1  | [[x]](Clojure/day01.clj)
 2  | [[x]](Clojure/day02.clj)
 3  | [[x]](Clojure/day03.clj) 
 4  | [[x]](Clojure/day04.clj) 
 5  | [ ]
 6  | [[x]](Clojure/day06.clj) 
 7  | [[x]](Clojure/day07.clj) 
 8  | [[x]](Clojure/day08.clj) 
 9  | [[x]](Clojure/day09.clj)
10  | [[x]](Clojure/day10.clj)
11  | [ ]
12  | [[x]](Clojure/day12.clj)

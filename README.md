ERStar
======

*Almost* fully conforming implementation of the dynamic R* Tree in-memory data structure in plain Erlang.

If you want theoretical details, consider consult a [paper](http://dbs.mathematik.uni-marburg.de/publications/myPapers/1990/BKSS90.pdf) on the subject.

Installation
------------

You will need a working copy of [rebar](https://github.com/rebar/rebar) to make things work.

It is very straightforward to add oneself as a dependency to your project, just add following line to your `rebar.config`:

```
{deps, [
    ...
    {lager, ".*", {git, "git://github.com/basho/lager.git", "master"}}
]}.
```

Usage
-----

Use the exported functions of the `erstar`, it is the main module of the library.
Module `erstar_bound` provides types and means to work with 2-dimensional bounding boxes.
Module `erstar_utils` just export tree structures to an SVG document, to ease perception of them.

Following snippet of code creates an empty tree and populates it with a pair of point data records and a rectangular one, and then searches for the first record met in a radial surroundings of the specific point:

```
RTree0 = erstar:new(32),                             %% maximum node capacity
RTree1 = erstar:insert(erstar_bound:new(12, 31.5), point_data_1, RTree0),
RTree2 = erstar:insert(erstar_bound:new(-20.6, 37.1), <<"point_data_2">>, RTree1),
RTree3 = erstar:insert(erstar_bound:new(1.1, 18, 5, 5), {rect_data, 3}, RTree2),
[First | _] = erstar:around(1.2, 13, 8.0, RTree3),   %% should find only last one inserted
```

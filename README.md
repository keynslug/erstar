

# ERStar #

Copyright (c) 2013 Andrew Majorov

__Version:__ 0.1.3

__Authors:__ Andrew Majorov ([`encube.ul@gmail.com`](mailto:encube.ul@gmail.com)).


### <a name="ERStar">ERStar</a> ###

_Almost_ fully conforming implementation of the dynamic R* Tree in-memory data structure in plain Erlang.

If you want theoretical details, consider consult a [paper](http://dbs.mathematik.uni-marburg.de/publications/myPapers/1990/BKSS90.pdf) on the subject.


#### <a name="Installation">Installation</a> ####

You will need a working copy of [rebar](https://github.com/rebar/rebar) to make things work.

It is very straightforward to add oneself as a dependency to your project, just add following line to your `rebar.config`:

```
{deps, [
    ...
    {erstar, ".*", {git, "git://github.com/keynslug/erstar.git", "master"}}
]}.
```


#### <a name="Usage">Usage</a> ####

Use the exported functions of the `erstar`, it is the main module of the library.
Module `erstar_bound` provides types and means to work with 2-dimensional bounding boxes.
Module `erstar_utils` just export tree structures to an SVG document, to ease perception of them.

Following snippet of code creates an empty tree and populates it with a pair of point data records and a rectangular one, and then searches for the first record met in a radial surroundings of the specific point:

```
RTree0 = erstar:new(32),                             %% maximum node capacity
RTree1 = erstar:insert(erstar_bound:new(12, 31.5), point_data_1, RTree0),
RTree2 = erstar:insert(erstar_bound:new(-20.6, 37.1), &#171;"point_data_2"&#187;, RTree1),
RTree3 = erstar:insert(erstar_bound:new(1.1, 18, 5, 5), {rect_data, 3}, RTree2),
[First | _] = erstar:around(1.2, 13, 8.0, RTree3),   %% should find only last one inserted
```


<script>
// Jump directly to a referenced url given in trailing '[]:...'-notation
function goto(tag) { parent.document.location.href = url(tag); }
function url(tag) { var o=document.getElementById(tag); return o ? o.href : '#'+tag; }
</script>



## Modules ##


<table width="100%" border="0" summary="list of modules">
<tr><td><a href="https://github.com/keynslug/erstar/blob/master/doc/erstar.md" class="module">erstar</a></td></tr>
<tr><td><a href="https://github.com/keynslug/erstar/blob/master/doc/erstar_bound.md" class="module">erstar_bound</a></td></tr>
<tr><td><a href="https://github.com/keynslug/erstar/blob/master/doc/erstar_geo.md" class="module">erstar_geo</a></td></tr>
<tr><td><a href="https://github.com/keynslug/erstar/blob/master/doc/erstar_metrics.md" class="module">erstar_metrics</a></td></tr>
<tr><td><a href="https://github.com/keynslug/erstar/blob/master/doc/erstar_svg.md" class="module">erstar_svg</a></td></tr>
<tr><td><a href="https://github.com/keynslug/erstar/blob/master/doc/erstar_utils.md" class="module">erstar_utils</a></td></tr></table>


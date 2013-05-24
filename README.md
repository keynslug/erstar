

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


#### <a name="Queries">Queries</a> ####

Erstar supports four types of queries to find stored data by its location:

* point query (`at/3`) - returns tree leaf right under the given point.
* rectangle query (`locate/2`, `locate/3`) - returns all tree leaves falling into given rectangular bound, either fully enclosed inside or crossed over just a part of it (depending on the first argument in `locate/3`).
* circular query (`around/4`) - returns all leaves which are closer than given distance to the given point, effectively falling into circle with the distance as its radius.
* annulus query (`inbetween/5`) - returns all leaves which are farther than first given distance but closer than second one to the given point, effectively falling into annulus with distances as its small and great radiuses.


#### <a name="Geo_locations">Geo locations</a> ####

Erstar supports storing geospatial information expressed in terms of spherical coordinates, commonly known as latitude and longitude, with respect of sphere curvature. These are widely used to express locations on the surface of the Earth. All the hard work done by `erstar_geo` module.

This module supports two location queries, aware of spherical coordinates and curvature:

* circular query (`around/4`) - returns all leaves which are closer than given distance to the given point, where distance is measured along the closest path between two points on the surface of sphere.
* annulus query (`inbetween/5`) - returns all leaves which are farther than first given distance but closer than second one to the given point, where distance is measured along the closest path between two points on the surface of sphere.

The closest path distance between two points is known as [great-circle distance](http://en.wikipedia.org/wiki/Great-circle_distance).

To properly use these queries one should assure that all locations in a tree are inside valid ranges, otherwise query results may be incorrect. When expressed in degrees, valid range for latitude is from -90 to 90, and for longitude is from -180 to 180.


#### <a name="Native_extensions">Native extensions</a> ####

Erstar provides a way to enable native extensions through NIFs. These are designed to speed up a couple of most time-consuming yet small-scale activities. For now, it is not supposed to fully reimplement Erstar through NIFs.

Speedups are notable: insertion times are 2 to 10 times faster depending on a tree parameters and geolocation queries are 2 to 3 times faster. However, these magnitudes are approximate and measured on the synthetic datasets, thus one should decide to use them or not on its own.

Native extensions are enabled with `ERSTAR_NIFEXT` env variable, like that:

```
ERSTAR_NIFEXT=1 make
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


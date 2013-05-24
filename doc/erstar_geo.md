

# Module erstar_geo #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)


A several features to ease using erstar as a geospatial data structure.

<a name="description"></a>

## Description ##



Assumes that any coordinates given are geospatial, i.e. in two-dimensional spherical
coordinate system, expressed in radians. Any distances are spherical angles of great
circle arcs, again expressed in radians.


Please note, that computations performed are far more complex than these in general
purpose queries.<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#around-4">around/4</a></td><td>Locates all the leaves which are closer than <code>CloserThan</code> radians to
the given point on a sphere.</td></tr><tr><td valign="top"><a href="#deg_to_rad-1">deg_to_rad/1</a></td><td>Converts degrees to radians.</td></tr><tr><td valign="top"><a href="#distance-4">distance/4</a></td><td>Measures distance between two points on a sphere, given its spherical
coordinates in radians.</td></tr><tr><td valign="top"><a href="#inbetween-5">inbetween/5</a></td><td>Locates all the leaves which are in between <code>FartherThan` and `CloserThan</code>
radians far from the given point on a sphere.</td></tr><tr><td valign="top"><a href="#rad_to_deg-1">rad_to_deg/1</a></td><td>Converts radians to degrees.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="around-4"></a>

### around/4 ###


<pre><code>around(Phi::number(), Lambda::number(), CloserThan::number(), RStar::<a href="erstar.md#type-rtree">erstar:rtree()</a>) -&gt; [<a href="erstar.md#type-treeleaf">erstar:treeleaf()</a>]</code></pre>

<br></br>


Locates all the leaves which are closer than `CloserThan` radians to
the given point on a sphere.
Distance is measured along the shortest path, which is an arc of great circle
connecting two points.
Distance to a bound is said to be the distance to its midpoint.
<a name="deg_to_rad-1"></a>

### deg_to_rad/1 ###


<pre><code>deg_to_rad(A::number()) -&gt; float()</code></pre>

<br></br>


Converts degrees to radians.
<a name="distance-4"></a>

### distance/4 ###


<pre><code>distance(Phi1::number(), Lambda1::number(), Phi2::number(), Lambda2::number()) -&gt; number()</code></pre>

<br></br>


Measures distance between two points on a sphere, given its spherical
coordinates in radians. Returned value expressed in great circle arc radians.
<a name="inbetween-5"></a>

### inbetween/5 ###


<pre><code>inbetween(Phi::number(), Lambda::number(), FartherThan::number(), CloserThan::number(), RStar::<a href="erstar.md#type-rtree">erstar:rtree()</a>) -&gt; [<a href="erstar.md#type-treeleaf">erstar:treeleaf()</a>]</code></pre>

<br></br>


Locates all the leaves which are in between `FartherThan` and `CloserThan`
radians far from the given point on a sphere.
Distance is measured along the shortest path, which is an arc of great circle
connecting two points.
Distance to a bound is said to be the distance to its midpoint.
<a name="rad_to_deg-1"></a>

### rad_to_deg/1 ###


<pre><code>rad_to_deg(A::number()) -&gt; float()</code></pre>

<br></br>


Converts radians to degrees.



# Module erstar_metrics #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)


Small collection of functions useful to measure some aspects of R* trees.

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#depth-1">depth/1</a></td><td>Computes depth of an R* tree.</td></tr><tr><td valign="top"><a href="#total_area_by_level-1">total_area_by_level/1</a></td><td>Computes total area of all nodes at each level of an R* tree.</td></tr><tr><td valign="top"><a href="#total_overlap_by_level-1">total_overlap_by_level/1</a></td><td>Computes total overlapping area of all nodes at each level of an R* tree.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="depth-1"></a>

### depth/1 ###


<pre><code>
depth(RTree::<a href="erstar.md#type-rtree">erstar:rtree()</a>) -&gt; pos_integer()
</code></pre>

<br></br>



Computes depth of an R* tree.


That is a level where leafs reside assuming level of the root node is 1.
<a name="total_area_by_level-1"></a>

### total_area_by_level/1 ###


<pre><code>
total_area_by_level(RTree::<a href="erstar.md#type-rtree">erstar:rtree()</a>) -&gt; [{pos_integer(), number()}]
</code></pre>

<br></br>



Computes total area of all nodes at each level of an R* tree.


Obviously, total area is decreasing with each level down a tree.
Lesser values mean more effective storage utilization.
<a name="total_overlap_by_level-1"></a>

### total_overlap_by_level/1 ###


<pre><code>
total_overlap_by_level(RTree::<a href="erstar.md#type-rtree">erstar:rtree()</a>) -&gt; [{pos_integer(), number()}]
</code></pre>

<br></br>



Computes total overlapping area of all nodes at each level of an R* tree.


Total overlapping area is sum of overlapping areas of each bound with each
from the same level when each bound taken only once.
Lesser values mean more effective lookups and storage utilization.



# Module erstar #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)


Implementation of R* tree data structure.


<a name="types"></a>

## Data Types ##




### <a name="type-rtree">rtree()</a> ###



<pre><code>
rtree() = {'?MODULE', {pos_integer(), pos_integer(), pos_integer(), non_neg_integer()}, <a href="#type-treenode">treenode()</a>}
</code></pre>





### <a name="type-treeleaf">treeleaf()</a> ###



<pre><code>
treeleaf() = {<a href="erstar_bound.md#type-bound">erstar_bound:bound()</a>, any()}
</code></pre>





### <a name="type-treenode">treenode()</a> ###



<pre><code>
treenode() = {node, empty | <a href="erstar_bound.md#type-bound">erstar_bound:bound()</a>, [<a href="#type-treenode">treenode()</a> | <a href="#type-treeleaf">treeleaf()</a>]}
</code></pre>


<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#around-4">around/4</a></td><td>Locates all the leaves which are closer than <code>CloserThan</code> units to
the given point.</td></tr><tr><td valign="top"><a href="#at-3">at/3</a></td><td>Locates all the leaves which contain the given point in its bound.</td></tr><tr><td valign="top"><a href="#fold-3">fold/3</a></td><td>Folds over entire tree in a depth-first traversal.</td></tr><tr><td valign="top"><a href="#foldwide-3">foldwide/3</a></td><td>Folds over entire tree in a breadth-first traversal.</td></tr><tr><td valign="top"><a href="#inbetween-5">inbetween/5</a></td><td>Locates all the leaves which are in between <code>FartherThan</code> and <code>ButCloserThan</code>
units far from the given point.</td></tr><tr><td valign="top"><a href="#insert-2">insert/2</a></td><td>Inserts a bulk of leafs simultaneously.</td></tr><tr><td valign="top"><a href="#insert-3">insert/3</a></td><td>Inserts new leaf into an R* tree, given its bound and arbitrary term
to associate with it.</td></tr><tr><td valign="top"><a href="#instance-1">instance/1</a></td><td>Verifies that the given term is an R* tree.</td></tr><tr><td valign="top"><a href="#leaves-1">leaves/1</a></td><td>Gathers plain list of all leaves in an R* tree.</td></tr><tr><td valign="top"><a href="#locate-2">locate/2</a></td><td>Locates all the leaves getting inside the given bound.</td></tr><tr><td valign="top"><a href="#locate-3">locate/3</a></td><td>Locates all the leaves enclosed inside or overlapping the given bound.</td></tr><tr><td valign="top"><a href="#new-1">new/1</a></td><td>Creates an empty R* tree with specific maximum node capacity.</td></tr><tr><td valign="top"><a href="#new-2">new/2</a></td><td>Creates an empty R* tree with specific minimum and maximum node capacity.</td></tr><tr><td valign="top"><a href="#new-4">new/4</a></td><td>Creates an empty R* tree with specific node capacities, choose-subtree cutout
value and reinserts count.</td></tr><tr><td valign="top"><a href="#remove-2">remove/2</a></td><td>Removes the bulk of leaves with a single call.</td></tr><tr><td valign="top"><a href="#remove-3">remove/3</a></td><td>Removes the leaf from an R* tree, given its bound and arbitrary term.</td></tr><tr><td valign="top"><a href="#size-1">size/1</a></td><td>Computes the size of an R* tree.</td></tr><tr><td valign="top"><a href="#walk-2">walk/2</a></td><td>Walks over nodes and leaves in a tree, effectively gathering list of
leaves accepted by the user-defined function.</td></tr><tr><td valign="top"><a href="#walkfold-3">walkfold/3</a></td><td>Walks over nodes and leaves in a tree, simultaneously folding over these
which were accepted by the user-defined function.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="around-4"></a>

### around/4 ###


<pre><code>
around(X::number(), Y::number(), CloserThan::number(), RStar::<a href="#type-rtree">rtree()</a>) -&gt; [<a href="#type-treeleaf">treeleaf()</a>]
</code></pre>

<br></br>


Locates all the leaves which are closer than `CloserThan` units to
the given point.
Distance to a bound is said to be the distance to its center.
<a name="at-3"></a>

### at/3 ###


<pre><code>
at(X::number(), Y::number(), RStar::<a href="#type-rtree">rtree()</a>) -&gt; [<a href="#type-treeleaf">treeleaf()</a>]
</code></pre>

<br></br>


Locates all the leaves which contain the given point in its bound.

__See also:__ [locate/3](#locate-3).
<a name="fold-3"></a>

### fold/3 ###


<pre><code>
fold(FoldFun, Acc, X3::<a href="#type-rtree">rtree()</a>) -&gt; Acc
</code></pre>

<ul class="definitions"><li><code>FoldFun = fun((node | leaf, <a href="erstar_bound.md#type-bound">erstar_bound:bound()</a>, any(), pos_integer(), Acc) -&gt; Acc)</code></li><li><code>Acc = any()</code></li></ul>

Folds over entire tree in a depth-first traversal.
Accumulates result with each call to the user-defined function. Each call is
given entry type, `node` or `leaf`, its bound, the thing it contain, numeric
level in a tree and the accumulator.
Leaves contain arbitrary terms passed to `insert` while nodes contain list of
their children.
<a name="foldwide-3"></a>

### foldwide/3 ###


<pre><code>
foldwide(FoldFun, Acc, X3::<a href="#type-rtree">rtree()</a>) -&gt; Acc
</code></pre>

<ul class="definitions"><li><code>FoldFun = fun((node | leaf, <a href="erstar_bound.md#type-bound">erstar_bound:bound()</a>, any(), pos_integer(), Acc) -&gt; Acc)</code></li><li><code>Acc = any()</code></li></ul>

Folds over entire tree in a breadth-first traversal.
Otherwise, the same as `fold/3`.

__See also:__ [fold/3](#fold-3).
<a name="inbetween-5"></a>

### inbetween/5 ###


<pre><code>
inbetween(X::number(), Y::number(), FartherThan::number(), ButCloserThan::number(), RStar::<a href="#type-rtree">rtree()</a>) -&gt; [<a href="#type-treeleaf">treeleaf()</a>]
</code></pre>

<br></br>


Locates all the leaves which are in between `FartherThan` and `ButCloserThan`
units far from the given point.
Distance to a bound is said to be the distance to its center.
<a name="insert-2"></a>

### insert/2 ###


<pre><code>
insert(Leafs::[<a href="#type-treeleaf">treeleaf()</a>], RStar::<a href="#type-rtree">rtree()</a>) -&gt; <a href="#type-rtree">rtree()</a>
</code></pre>

<br></br>


Inserts a bulk of leafs simultaneously.
Each leaf is a tuple containing its bound and arbitrary term.

__See also:__ [insert/3](#insert-3).
<a name="insert-3"></a>

### insert/3 ###


<pre><code>
insert(Bound::<a href="erstar_bound.md#type-bound">erstar_bound:bound()</a>, Data::any(), RStar::<a href="#type-rtree">rtree()</a>) -&gt; <a href="#type-rtree">rtree()</a>
</code></pre>

<br></br>



Inserts new leaf into an R* tree, given its bound and arbitrary term
to associate with it.


Generally, it is expensive operation. If you expect large number of inserts
with your usage plan, consider lowering reinserts count or maximum node
capacity, or both.
<a name="instance-1"></a>

### instance/1 ###


<pre><code>
instance(X1::any()) -&gt; boolean()
</code></pre>

<br></br>


Verifies that the given term is an R* tree.
<a name="leaves-1"></a>

### leaves/1 ###


<pre><code>
leaves(RTree::<a href="#type-rtree">rtree()</a>) -&gt; [<a href="#type-treeleaf">treeleaf()</a>]
</code></pre>

<br></br>


Gathers plain list of all leaves in an R* tree.
<a name="locate-2"></a>

### locate/2 ###


<pre><code>
locate(Where::<a href="erstar_bound.md#type-bound">erstar_bound:bound()</a>, RStar::<a href="#type-rtree">rtree()</a>) -&gt; [<a href="#type-treeleaf">treeleaf()</a>]
</code></pre>

<br></br>


Locates all the leaves getting inside the given bound.

__See also:__ [locate/3](#locate-3).
<a name="locate-3"></a>

### locate/3 ###


<pre><code>
locate(X1::enclose | intersect, Where::<a href="erstar_bound.md#type-bound">erstar_bound:bound()</a>, RStar::<a href="#type-rtree">rtree()</a>) -&gt; [<a href="#type-treeleaf">treeleaf()</a>]
</code></pre>

<br></br>


Locates all the leaves enclosed inside or overlapping the given bound.
Thus, `enclose` and `intersect` specify which set will be returned, respectively.
<a name="new-1"></a>

### new/1 ###


<pre><code>
new(MaxCapacity::pos_integer()) -&gt; <a href="#type-rtree">rtree()</a>
</code></pre>

<br></br>


Creates an empty R* tree with specific maximum node capacity.
Same as to create a tree with minimum node capacity equal to 40% of the
`MaxCapacity`.

__See also:__ [new/2](#new-2).
<a name="new-2"></a>

### new/2 ###


<pre><code>
new(MinCapacity::pos_integer(), MaxCapacity::pos_integer()) -&gt; <a href="#type-rtree">rtree()</a>
</code></pre>

<br></br>


Creates an empty R* tree with specific minimum and maximum node capacity.
Same as to create a tree with given node capacities, choose-subtree cutout of 32
and reinserts count equal to 30% of the `MaxCapacity`.

__See also:__ [new/4](#new-4).
<a name="new-4"></a>

### new/4 ###


<pre><code>
new(MinCapacity, MaxCapacity, ChooseCutout::pos_integer(), ReinsertCount::non_neg_integer()) -&gt; <a href="#type-rtree">rtree()</a>
</code></pre>

<ul class="definitions"><li><code>MinCapacity = pos_integer()</code></li><li><code>MaxCapacity = pos_integer()</code></li></ul>


Creates an empty R* tree with specific node capacities, choose-subtree cutout
value and reinserts count.



Node capacities dictate how many children each node should contain. Only the root
node allowed to contain lesser than `MinCapacity` number of children.
Minimum node capacity should be at least 2 and maximum capacity should be at
least double the minimum one. Practically, in the most of cases wide nodes
are preferred, starting from 16 or 32, and wider.



Choose-subtree cutout value says how many children at each level are being
considered during each insertion. Generally, the lesser this value is, the
faster each insert will be, but later lookups may degrade in performance
instead.



Finally, reinserts count states how many nodes should be inserted again during
each node split. Theretically, greater values would give better R* tree
lookup performance in the long term. But on the other hand they will incur
notable insert performace penalty. However, author's observations show that
there was no notable improvement in the tree quality on different, even pretty big,
datasets when recommended values were used.


As paper on the subject states, the general optimal value for the minimum node
capacity is 40% of the maximum one, for the choose-subtree cutout is 32 and for
the reinserts count is at 30% of the maximum node capacity.
<a name="remove-2"></a>

### remove/2 ###


<pre><code>
remove(Leaves::[<a href="#type-treeleaf">treeleaf()</a>], RStar::<a href="#type-rtree">rtree()</a>) -&gt; <a href="#type-rtree">rtree()</a>
</code></pre>

<br></br>


Removes the bulk of leaves with a single call.

__See also:__ [remove/3](#remove-3).
<a name="remove-3"></a>

### remove/3 ###


<pre><code>
remove(Bound::<a href="erstar_bound.md#type-bound">erstar_bound:bound()</a>, Data::any(), RStar::<a href="#type-rtree">rtree()</a>) -&gt; <a href="#type-rtree">rtree()</a>
</code></pre>

<br></br>


Removes the leaf from an R* tree, given its bound and arbitrary term.
Does nothing if tree does not contain such leaf.
<a name="size-1"></a>

### size/1 ###


<pre><code>
size(RTree::<a href="#type-rtree">rtree()</a>) -&gt; non_neg_integer()
</code></pre>

<br></br>



Computes the size of an R* tree.


Please note that the computation involves traversal
of the whole tree.
<a name="walk-2"></a>

### walk/2 ###


<pre><code>
walk(WalkFun, X2::<a href="#type-rtree">rtree()</a>) -&gt; [<a href="#type-treeleaf">treeleaf()</a>]
</code></pre>

<ul class="definitions"><li><code>WalkFun = fun((node | leaf, <a href="erstar_bound.md#type-bound">erstar_bound:bound()</a>) -&gt; boolean())</code></li></ul>

Walks over nodes and leaves in a tree, effectively gathering list of
leaves accepted by the user-defined function.
User-defined function should decide, given entry type and its bound, if a node
should be visited or a leaf should appear in the result.
If you want to issue some specific locate query on a tree, start here.
<a name="walkfold-3"></a>

### walkfold/3 ###


<pre><code>
walkfold(WalkFun, Acc, X3::<a href="#type-rtree">rtree()</a>) -&gt; Acc
</code></pre>

<ul class="definitions"><li><code>WalkFun = fun((node | leaf, <a href="erstar_bound.md#type-bound">erstar_bound:bound()</a>, any(), pos_integer(), Acc) -&gt; Result)</code></li><li><code>Result = {ok, Acc} | {descend, Acc} | {done, Acc}</code></li><li><code>Acc = any()</code></li></ul>


Walks over nodes and leaves in a tree, simultaneously folding over these
which were accepted by the user-defined function.
User-defined function should decide, given entry type, its bound, thing it contains,
its level in a tree and the accumulator, what it wants to do further. If `{ok, Acc}`
is returned, the walk operation continues over other siblings in a tree. On the other
hand, if `{descend, Acc}` is returned, operation continues with all children of
the current node. It is not allowed to `descend` when walking over a leaf. Finally,
`{done, Acc}` ends the walk operation instantly, returning `Acc` as the final result.


This operation is a hybrid of `fold/3` and `walk/2`.

__See also:__ [fold/3](#fold-3), [walk/2](#walk-2).



# Module erstar_bound #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)


Module provides means to deal with simple structures called bounds.

<a name="description"></a>

## Description ##
  Each such bound contain description of a two-dimensional bounding box.
<a name="types"></a>

## Data Types ##




### <a name="type-bound">bound()</a> ###



<pre><code>bound() = {number(), number(), number(), number()}</code></pre>


<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#area-1">area/1</a></td><td>Computes area of a bound.</td></tr><tr><td valign="top"><a href="#center-1">center/1</a></td><td>Returns effective coordinates of the center of a bound.</td></tr><tr><td valign="top"><a href="#dimensions-1">dimensions/1</a></td><td>Returns effective dimensions of a bound.</td></tr><tr><td valign="top"><a href="#empty-0">empty/0</a></td><td>Creates an empty bound, with no position and dimensions.</td></tr><tr><td valign="top"><a href="#intersect-2">intersect/2</a></td><td>Returns new bound comprised of all common points of two specified bound.</td></tr><tr><td valign="top"><a href="#lowerleft-1">lowerleft/1</a></td><td>Returns coordinates of the lower left point of a bound.</td></tr><tr><td valign="top"><a href="#margin-1">margin/1</a></td><td>Computes margin of a bound.</td></tr><tr><td valign="top"><a href="#new-2">new/2</a></td><td>Creates bound with position and no dimensions, effectively a point.</td></tr><tr><td valign="top"><a href="#new-4">new/4</a></td><td>Creates bound with well-defined position and dimensions.</td></tr><tr><td valign="top"><a href="#overlap-2">overlap/2</a></td><td>Computes area covered by the intersection of two specified bounds.</td></tr><tr><td valign="top"><a href="#unify-2">unify/2</a></td><td>Returns new bound containing each one of specified bounds.</td></tr><tr><td valign="top"><a href="#upperright-1">upperright/1</a></td><td>Returns coordinates of the upper right point of a bound.</td></tr><tr><td valign="top"><a href="#x1-1">x1/1</a></td><td>Returns lower coordinate of a bound on the X axis.</td></tr><tr><td valign="top"><a href="#x2-1">x2/1</a></td><td>Returns upper coordinate of a bound on the X axis.</td></tr><tr><td valign="top"><a href="#y1-1">y1/1</a></td><td>Returns lower coordinate of a bound on the Y axis.</td></tr><tr><td valign="top"><a href="#y2-1">y2/1</a></td><td>Returns upper coordinate of a bound on the Y axis.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="area-1"></a>

### area/1 ###


<pre><code>area(X1::empty | <a href="#type-bound">bound()</a>) -&gt; number()</code></pre>

<br></br>


Computes area of a bound.
<a name="center-1"></a>

### center/1 ###


<pre><code>center(X1::<a href="#type-bound">bound()</a>) -&gt; {number(), number()}</code></pre>

<br></br>


Returns effective coordinates of the center of a bound.
<a name="dimensions-1"></a>

### dimensions/1 ###


<pre><code>dimensions(X1::<a href="#type-bound">bound()</a>) -&gt; {number(), number()}</code></pre>

<br></br>


Returns effective dimensions of a bound.
<a name="empty-0"></a>

### empty/0 ###


<pre><code>empty() -&gt; empty</code></pre>

<br></br>


Creates an empty bound, with no position and dimensions.
<a name="intersect-2"></a>

### intersect/2 ###


<pre><code>intersect(X1::<a href="#type-bound">bound()</a>, X2::<a href="#type-bound">bound()</a>) -&gt; empty | <a href="#type-bound">bound()</a></code></pre>

<br></br>


Returns new bound comprised of all common points of two specified bound.
The resulting bound can be empty, in case there are no common points at all.
<a name="lowerleft-1"></a>

### lowerleft/1 ###


<pre><code>lowerleft(X1::<a href="#type-bound">bound()</a>) -&gt; {number(), number()}</code></pre>

<br></br>


Returns coordinates of the lower left point of a bound.
<a name="margin-1"></a>

### margin/1 ###


<pre><code>margin(X1::empty | <a href="#type-bound">bound()</a>) -&gt; number()</code></pre>

<br></br>


Computes margin of a bound.
Margin is equal to the half of perimeter.
<a name="new-2"></a>

### new/2 ###


<pre><code>new(X::number(), Y::number()) -&gt; <a href="#type-bound">bound()</a></code></pre>

<br></br>


Creates bound with position and no dimensions, effectively a point.
<a name="new-4"></a>

### new/4 ###


<pre><code>new(X::number(), Y::number(), W::number(), H::number()) -&gt; <a href="#type-bound">bound()</a></code></pre>

<br></br>


Creates bound with well-defined position and dimensions.
Negative dimensions are not allowed.
<a name="overlap-2"></a>

### overlap/2 ###


<pre><code>overlap(X1::<a href="#type-bound">bound()</a>, X2::<a href="#type-bound">bound()</a>) -&gt; number()</code></pre>

<br></br>


Computes area covered by the intersection of two specified bounds.
If these bounds have no intersection, the result will be 0.
<a name="unify-2"></a>

### unify/2 ###


<pre><code>unify(B0::empty | <a href="#type-bound">bound()</a>, B1::empty | <a href="#type-bound">bound()</a>) -&gt; empty | <a href="#type-bound">bound()</a></code></pre>

<br></br>


Returns new bound containing each one of specified bounds.
The resulting bound is optimal one in terms of area.
<a name="upperright-1"></a>

### upperright/1 ###


<pre><code>upperright(X1::<a href="#type-bound">bound()</a>) -&gt; {number(), number()}</code></pre>

<br></br>


Returns coordinates of the upper right point of a bound.
<a name="x1-1"></a>

### x1/1 ###


<pre><code>x1(X1::<a href="#type-bound">bound()</a>) -&gt; number()</code></pre>

<br></br>


Returns lower coordinate of a bound on the X axis.
<a name="x2-1"></a>

### x2/1 ###


<pre><code>x2(X1::<a href="#type-bound">bound()</a>) -&gt; number()</code></pre>

<br></br>


Returns upper coordinate of a bound on the X axis.
<a name="y1-1"></a>

### y1/1 ###


<pre><code>y1(X1::<a href="#type-bound">bound()</a>) -&gt; number()</code></pre>

<br></br>


Returns lower coordinate of a bound on the Y axis.
<a name="y2-1"></a>

### y2/1 ###


<pre><code>y2(X1::<a href="#type-bound">bound()</a>) -&gt; number()</code></pre>

<br></br>


Returns upper coordinate of a bound on the Y axis.

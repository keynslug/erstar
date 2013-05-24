

# Module erstar_utils #
* [Function Index](#index)
* [Function Details](#functions)


<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#render-1">render/1</a></td><td>Visualises any R* tree in an opaque internal format.</td></tr><tr><td valign="top"><a href="#render-2">render/2</a></td><td>Visualises any R* tree in an external format.</td></tr><tr><td valign="top"><a href="#render_to_file-3">render_to_file/3</a></td><td>Visualises any R* tree in an external format and writes result to a file.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="render-1"></a>

### render/1 ###


<pre><code>render(RStar::<a href="erstar.md#type-rtree">erstar:rtree()</a>) -&gt; list()</code></pre>

<br></br>


Visualises any R* tree in an opaque internal format.
<a name="render-2"></a>

### render/2 ###


<pre><code>render(To::svg, RStar::<a href="erstar.md#type-rtree">erstar:rtree()</a>) -&gt; iolist()</code></pre>

<br></br>


Visualises any R* tree in an external format.
The only available external format is `svg`.
<a name="render_to_file-3"></a>

### render_to_file/3 ###


<pre><code>render_to_file(Filename::string(), To::svg, RStar::<a href="erstar.md#type-rtree">erstar:rtree()</a>) -&gt; ok | {error, atom()}</code></pre>

<br></br>


Visualises any R* tree in an external format and writes result to a file.
The only available external format is `svg`.

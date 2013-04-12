%%
%% SVG Render

-module(erstar_svg).

-export([
    render/1
]).

%%

-spec render(list()) -> iolist().

render(List) when length(List) > 0 ->
    render(List, [], erstar_bound:empty()).

render([Prim | Rest], Acc, Extent) ->
    PrimRender = render_prim(Prim),
    PrimExtent = get_prim_extent(Prim),
    render(Rest, [PrimRender | Acc], erstar_bound:unify(Extent, PrimExtent));

render([], Acc, Extent) ->
    {X, Y} = erstar_bound:lowerleft(Extent),
    {W, H} = erstar_bound:dimensions(Extent),
    Padding = max(W, H) / 20,
    Header = header(X - Padding, Y - Padding, W + Padding * 2, H + Padding * 2),
    [Header, lists:reverse(Acc), footer()].

%%

render_prim({bound, Bound, Level}) ->
    CB = get_level_color(Level),
    {X, Y} = erstar_bound:lowerleft(Bound),
    {W, H} = erstar_bound:dimensions(Bound),
    WC = max(eps(), W),
    HC = max(eps(), H),
    ExtentsList = io_lib:format("x=\"~p\" y=\"~p\" width=\"~p\" height=\"~p\"", [X, Y, WC, HC]),
    Extents = iolist_to_binary(ExtentsList),
    Style = <<"fill:", CB/binary, ";fill-opacity:0.1; stroke:", CB/binary, ";stroke-opacity:0.8;">>,
    <<"<rect ", Extents/binary, " style=\"", Style/binary, "\"/>", $\n>>;

render_prim({rect, Bound}) ->
    {X, Y} = erstar_bound:lowerleft(Bound),
    {W, H} = erstar_bound:dimensions(Bound),
    WC = max(eps(), W),
    HC = max(eps(), H),
    ExtentsList = io_lib:format("x=\"~p\" y=\"~p\" width=\"~p\" height=\"~p\"", [X, Y, WC, HC]),
    Extents = iolist_to_binary(ExtentsList),
    Style = <<"fill:#f00;fill-opacity:0.4;stroke:#f00;stroke-width:2;">>,
    <<"<rect ", Extents/binary, " rx=\"2\" style=\"", Style/binary, "\"/>", $\n>>;

render_prim({circle, {X, Y}, R}) ->
    Coords = iolist_to_binary(io_lib:format("cx=\"~p\" cy=\"~p\" r=\"~p\"", [X, Y, R])),
    <<"<circle ", Coords/binary, " style=\"stroke:#f00;fill:#f00;fill-opacity:0.4;\"/>", $\n>>;

render_prim({point, {X, Y}}) ->
    Coords = iolist_to_binary(io_lib:format("cx=\"~p\" cy=\"~p\"", [X, Y])),
    <<"<circle ", Coords/binary, " r=\"1\" style=\"stroke:#f00; fill:#f00;\"/>", $\n>>.

%%

get_prim_extent({bound, Bound, _}) ->
    Bound;

get_prim_extent({rect, Bound}) ->
    Bound;

get_prim_extent({circle, {X, Y}, R}) ->
    erstar_bound:new(X - R, Y - R, R * 2, R * 2);

get_prim_extent({point, {X, Y}}) ->
    erstar_bound:new(X, Y).

%%

header(X, Y, W, H) ->
    ViewBox = iolist_to_binary(io_lib:format("~p ~p ~p ~p", [X, Y, W, H])),
    Dims = iolist_to_binary(io_lib:format("width=\"~p\" height=\"~p\"", [W, H])),
    <<
        "<?xml version=\"1.0\" standalone=\"no\" ?>", $\n,
        "<svg ", Dims/binary, " viewBox=\"", ViewBox/binary, "\" xmlns=\"http://www.w3.org/2000/svg\">", $\n
    >>.

footer() ->
    <<"</svg>", $\n>>.

%%

get_level_color(1) ->
    <<"#9c0">>;

get_level_color(2) ->
    <<"#c90">>;

get_level_color(3) ->
    <<"#90c">>;

get_level_color(4) ->
    <<"#c09">>;

get_level_color(5) ->
    <<"#09c">>;

get_level_color(6) ->
    <<"#0c9">>;

get_level_color(_) ->
    <<"#333">>.

%%

eps() ->
    1.0e-16.

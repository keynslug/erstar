%%
%% Bounding boxes

-module(erstar_bound).

%%

-export([
    empty/0,
    new/2,
    new/4,
    lowerleft/1,
    upperright/1,
    center/1,
    x1/1,
    y1/1,
    x2/1,
    y2/1,
    dimensions/1,
    area/1,
    margin/1,
    unify/2,
    intersect/2,
    overlap/2
]).

-type bound() :: {number(), number(), number(), number()}.

-export_type([bound/0]).

%%

-spec empty() -> empty.

empty() ->
    empty.

-spec new(number(), number()) -> bound().

new(X, Y) ->
    new(X, Y, 0, 0).

-spec new(number(), number(), number(), number()) -> bound().

new(X, Y, W, H) when W >= 0, H >= 0 ->
    {X, Y, W, H}.

-spec x1(bound()) -> number().

x1({X, _, _, _}) ->
    X.

-spec y1(bound()) -> number().

y1({_, Y, _, _}) ->
    Y.

-spec x2(bound()) -> number().

x2({X, _, W, _}) ->
    X + W.

-spec y2(bound()) -> number().

y2({_, Y, _, H}) ->
    Y + H.

-spec lowerleft(bound()) -> {number(), number()}.

lowerleft({X, Y, _, _}) ->
    {X, Y}.

-spec upperright(bound()) -> {number(), number()}.

upperright({X, Y, W, H}) ->
    {X + W, Y + H}.

-spec center(bound()) -> {number(), number()}.

center({X, Y, 0, 0}) ->
    {X, Y};

center({X, Y, W, H}) ->
    {X + W / 2.0, Y + H / 2.0}.

-spec dimensions(bound()) -> {number(), number()}.

dimensions({_, _, W, H}) ->
    {W, H}.

-spec area(bound()) -> number().

area({_, _, W, H}) ->
    W * H.

-spec margin(bound()) -> number().

margin({_, _, W, H}) ->
    W + H.

-spec unify(empty | bound(), empty | bound()) -> empty | bound().

unify(empty, empty) ->
    empty;

unify(B0, empty) ->
    B0;

unify(empty, B1) ->
    B1;

unify({X0, Y0, W0, H0}, {X1, Y1, W1, H1}) ->
    {X, W} = unify(X0, W0, X1, W1),
    {Y, H} = unify(Y0, H0, Y1, H1),
    {X, Y, W, H}.

unify(X0, L0, X1, L1) ->
    X = min(X0, X1),
    {X, max(X0 + L0, X1 + L1) - X}.

-spec intersect(bound(), bound()) -> empty | bound().

intersect({X0, Y0, W0, H0}, {X1, Y1, W1, H1}) ->
    {X, W} = intersect(X0, W0, X1, W1),
    if
        W < 0 ->
            empty;
        true ->
            {Y, H} = intersect(Y0, H0, Y1, H1),
            if
                H < 0 ->
                    empty;
                true ->
                    {X, Y, W, H}
            end
    end.

intersect(X0, L0, X1, L1) when X1 >= X0 ->
    {X1, min(L1, L0 - (X1 - X0))};

intersect(X1, L1, X0, L0) ->
    {X1, min(L1, L0 - (X1 - X0))}.

-spec overlap(bound(), bound()) -> number().

overlap({X0, Y0, W0, H0}, {X1, Y1, W1, H1}) ->
    W = overlap(X0, W0, X1, W1),
    if
        W =< 0 ->
            0;
        true ->
            H = overlap(Y0, H0, Y1, H1),
            if
                H =< 0 ->
                    0;
                true ->
                    W * H
            end
    end.

overlap(X0, L0, X1, L1) when X0 < X1 ->
    min(L1, L0 - (X1 - X0));

overlap(X1, L1, X0, L0) ->
    min(L1, L0 - (X1 - X0)).

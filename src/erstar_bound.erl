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
    {X, Y, X, Y}.

-spec new(number(), number(), number(), number()) -> bound().

new(X, Y, W, H) when W >= 0, H >= 0 ->
    {X, Y, X + W, Y + H}.

-spec x1(bound()) -> number().

x1({X, _, _, _}) ->
    X.

-spec y1(bound()) -> number().

y1({_, Y, _, _}) ->
    Y.

-spec x2(bound()) -> number().

x2({_, _, X, _}) ->
    X.

-spec y2(bound()) -> number().

y2({_, _, _, Y}) ->
    Y.

-spec lowerleft(bound()) -> {number(), number()}.

lowerleft({X, Y, _, _}) ->
    {X, Y}.

-spec upperright(bound()) -> {number(), number()}.

upperright({_, _, X, Y}) ->
    {X, Y}.

-spec center(bound()) -> {number(), number()}.

center({X, Y, X, Y}) ->
    {X, Y};

center({X1, Y1, X2, Y2}) ->
    {(X1 + X2) / 2.0, (Y1 + Y2) / 2.0}.

-spec dimensions(bound()) -> {number(), number()}.

dimensions({X1, Y1, X2, Y2}) ->
    {X2 - X1, Y2 - Y1}.

-spec area(bound()) -> number().

area({X1, Y1, X2, Y2}) ->
    (X2 - X1) * (Y2 - Y1).

-spec margin(bound()) -> number().

margin({X1, Y1, X2, Y2}) ->
    (X2 - X1) + (Y2 - Y1).

-spec unify(empty | bound(), empty | bound()) -> empty | bound().

unify(empty, empty) ->
    empty;

unify(B0, empty) ->
    B0;

unify(empty, B1) ->
    B1;

unify({Xa1, Ya1, Xa2, Ya2}, {Xb1, Yb1, Xb2, Yb2}) ->
    {min(Xa1, Xb1), min(Ya1, Yb1), max(Xa2, Xb2), max(Ya2, Yb2)}.

-spec intersect(bound(), bound()) -> empty | bound().

intersect({Xa1, Ya1, Xa2, Ya2}, {Xb1, Yb1, Xb2, Yb2}) ->
    {Xr1, Xr2} = intersect(Xa1, Xa2, Xb1, Xb2),
    if
        Xr2 < Xr1 ->
            empty;
        true ->
            {Yr1, Yr2} = intersect(Ya1, Ya2, Yb1, Yb2),
            if
                Yr2 < Yr1 ->
                    empty;
                true ->
                    {Xr1, Yr1, Xr2, Yr2}
            end
    end.

intersect(Xa1, Xa2, Xb1, Xb2) when Xa1 < Xb1 ->
    {Xb1, min(Xa2, Xb2)};

intersect(Xb1, Xb2, _Xa1, Xa2) ->
    {Xb1, min(Xa2, Xb2)}.

-spec overlap(bound(), bound()) -> number().

overlap({Xa1, Ya1, Xa2, Ya2}, {Xb1, Yb1, Xb2, Yb2}) ->
    Wr = overlap(Xa1, Xa2, Xb1, Xb2),
    if
        Wr =< 0 ->
            0;
        true ->
            Hr = overlap(Ya1, Ya2, Yb1, Yb2),
            if
                Hr =< 0 ->
                    0;
                true ->
                    Wr * Hr
            end
    end.

overlap(Xa1, Xa2, Xb1, Xb2) when Xa1 < Xb1 ->
    min(Xa2, Xb2) - Xb1;

overlap(Xb1, Xb2, _Xa1, Xa2) ->
    min(Xa2, Xb2) - Xb1.

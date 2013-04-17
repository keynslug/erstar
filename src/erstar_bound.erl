%% Copyright (c) 2013 Andrew Majorov <encube.ul@gmail.com>
%%
%% All Rights Reserved.
%%
%% Redistribution and use in source and binary forms, with or without modification,
%% are permitted provided that the following conditions are met:
%%
%% 1. Redistributions of source code must retain the above copyright notice, this
%%    list of conditions and the following disclaimer.
%%
%% 2. Redistributions in binary form must reproduce the above copyright notice,
%%    this list of conditions and the following disclaimer in the documentation
%%    and/or other materials provided with the distribution.
%%
%% THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS ``AS IS'' AND ANY
%% EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
%% WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
%% DISCLAIMED. IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE LIABLE FOR ANY
%% DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
%% (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
%% LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
%% ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
%% (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
%% SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

%%
%% Bounding boxes
%% @doc Module provides means to deal with simple structures called bounds.
%% Each such bound contain description of a two-dimensional bounding box.

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

%% @doc Creates an empty bound, with no position and dimensions.

-spec empty() -> empty.

empty() ->
    empty.

-spec new(number(), number()) -> bound().

%% @doc Creates bound with position and no dimensions, effectively a point.

new(X, Y) ->
    {X, Y, X, Y}.

-spec new(number(), number(), number(), number()) -> bound().

%% @doc Creates bound with well-defined position and dimensions.
%% Negative dimensions are not allowed.

new(X, Y, W, H) when W >= 0, H >= 0 ->
    {X, Y, X + W, Y + H}.

%% @doc Returns lower coordinate of a bound on the X axis.

-spec x1(bound()) -> number().

x1({X, _, _, _}) ->
    X.

%% @doc Returns lower coordinate of a bound on the Y axis.

-spec y1(bound()) -> number().

y1({_, Y, _, _}) ->
    Y.

%% @doc Returns upper coordinate of a bound on the X axis.

-spec x2(bound()) -> number().

x2({_, _, X, _}) ->
    X.

%% @doc Returns upper coordinate of a bound on the Y axis.

-spec y2(bound()) -> number().

y2({_, _, _, Y}) ->
    Y.

%% @doc Returns coordinates of the lower left point of a bound.

-spec lowerleft(bound()) -> {number(), number()}.

lowerleft({X, Y, _, _}) ->
    {X, Y}.

%% @doc Returns coordinates of the upper right point of a bound.

-spec upperright(bound()) -> {number(), number()}.

upperright({_, _, X, Y}) ->
    {X, Y}.

%% @doc Returns effective coordinates of the center of a bound.

-spec center(bound()) -> {number(), number()}.

center({X, Y, X, Y}) ->
    {X, Y};

center({X1, Y1, X2, Y2}) ->
    {(X1 + X2) / 2.0, (Y1 + Y2) / 2.0}.

%% @doc Returns effective dimensions of a bound.

-spec dimensions(bound()) -> {number(), number()}.

dimensions({X1, Y1, X2, Y2}) ->
    {X2 - X1, Y2 - Y1}.

%% @doc Computes area of a bound.

-spec area(empty | bound()) -> number().

area({X1, Y1, X2, Y2}) ->
    (X2 - X1) * (Y2 - Y1);

area(empty) ->
    0.

%% @doc Computes margin of a bound.
%% Margin is equal to the half of perimeter.

-spec margin(empty | bound()) -> number().

margin({X1, Y1, X2, Y2}) ->
    (X2 - X1) + (Y2 - Y1);

margin(empty) ->
    0.

%% @doc Returns new bound containing each one of specified bounds.
%% The resulting bound is optimal one in terms of area.

-spec unify(empty | bound(), empty | bound()) -> empty | bound().

unify(empty, empty) ->
    empty;

unify(B0, empty) ->
    B0;

unify(empty, B1) ->
    B1;

unify({Xa1, Ya1, Xa2, Ya2}, {Xb1, Yb1, Xb2, Yb2}) ->
    {min(Xa1, Xb1), min(Ya1, Yb1), max(Xa2, Xb2), max(Ya2, Yb2)}.

%% @doc Returns new bound comprised of all common points of two specified bound.
%% The resulting bound can be empty, in case there are no common points at all.

-spec intersect(bound(), bound()) -> empty | bound().

intersect({Xa1, Ya1, Xa2, Ya2}, {Xb1, Yb1, Xb2, Yb2}) ->
    Xr1 = max(Xa1, Xb1),
    Xr2 = min(Xa2, Xb2),
    if
        Xr2 < Xr1 ->
            empty;
        true ->
            Yr1 = max(Ya1, Yb1),
            Yr2 = min(Ya2, Yb2),
            if
                Yr2 < Yr1 ->
                    empty;
                true ->
                    {Xr1, Yr1, Xr2, Yr2}
            end
    end.

%% @doc Computes area covered by the intersection of two specified bounds.
%% If these bounds have no intersection, the result will be 0.

-spec overlap(bound(), bound()) -> number().

overlap({Xa1, Ya1, Xa2, Ya2}, {Xb1, Yb1, Xb2, Yb2}) ->
    Wr = min(Xa2, Xb2) - max(Xa1, Xb1),
    if
        Wr =< 0 ->
            0;
        true ->
            Hr = min(Ya2, Yb2) - max(Ya1, Yb1),
            if
                Hr =< 0 ->
                    0;
                true ->
                    Wr * Hr
            end
    end.

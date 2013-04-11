%%
%% R*-Tree metrics.

-module(erstar_metrics).

%%

-export([
    total_overlap_by_level/1,
    total_area_by_level/1
]).

%%

total_overlap_by_level(RTree) ->
    BoundsByLevel = erstar:fold(fun bounds_by_level/5, dict:new(), RTree),
    OverlapByLevel = overlap_by_level(BoundsByLevel),
    OverlapByLevel.

total_area_by_level(RTree) ->
    BoundsByLevel = erstar:fold(fun bounds_by_level/5, dict:new(), RTree),
    AreaByLevel = area_by_level(BoundsByLevel),
    AreaByLevel.

%%

bounds_by_level(leaf, _, _, _, Acc) ->
    Acc;

bounds_by_level(node, Bound, _, Level, Acc) ->
    dict:append(Level, Bound, Acc).

overlap_by_level(BoundsByLevel) ->
    Result = dict:fold(fun overlap_on_level/3, [], BoundsByLevel),
    lists:keysort(1, Result).

overlap_on_level(Level, Bounds, Acc) ->
    [{Level, overlap_on_level(0, Bounds)} | Acc].

overlap_on_level(R0, []) ->
    R0;

overlap_on_level(R0, [Bound | Rest]) ->
    Overlap = lists:sum([erstar_bound:overlap(Bound, B) || B <- Rest]),
    overlap_on_level(R0 + Overlap, Rest).

area_by_level(BoundsByLevel) ->
    Result = dict:fold(fun area_on_level/3, [], BoundsByLevel),
    lists:keysort(1, Result).

area_on_level(Level, Bounds, Acc) ->
    [{Level, lists:sum([erstar_bound:area(B) || B <- Bounds])} | Acc].

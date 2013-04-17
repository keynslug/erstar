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
%% R*-Tree metrics
%% @doc Small collection of functions useful to measure some aspects of R* trees.

-module(erstar_metrics).

%%

-export([
    depth/1,
    total_overlap_by_level/1,
    total_area_by_level/1
]).

%%

%% @doc Computes depth of an R* tree.
%%
%% That is a level where leafs reside assuming level of the root node is 1.

-spec depth(erstar:rtree()) -> pos_integer().

depth(RTree) ->
    erstar:walkfold(fun fold_depth/5, 1, RTree).

%% @doc Computes total overlapping area of all nodes at each level of an R* tree.
%%
%% Total overlapping area is sum of overlapping areas of each bound with each
%% from the same level when each bound taken only once.
%% Lesser values mean more effective lookups and storage utilization.

-spec total_overlap_by_level(erstar:rtree()) -> [{pos_integer(), number()}].

total_overlap_by_level(RTree) ->
    BoundsByLevel = erstar:fold(fun bounds_by_level/5, dict:new(), RTree),
    OverlapByLevel = overlap_by_level(BoundsByLevel),
    OverlapByLevel.

%% @doc Computes total area of all nodes at each level of an R* tree.
%%
%% Obviously, total area is decreasing with each level down a tree.
%% Lesser values mean more effective storage utilization.

-spec total_area_by_level(erstar:rtree()) -> [{pos_integer(), number()}].

total_area_by_level(RTree) ->
    BoundsByLevel = erstar:fold(fun bounds_by_level/5, dict:new(), RTree),
    AreaByLevel = area_by_level(BoundsByLevel),
    AreaByLevel.

%%

fold_depth(node, _, _, _, Acc) ->
    {descend, Acc};

fold_depth(leaf, _, _, Level, _) ->
    {done, Level}.

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

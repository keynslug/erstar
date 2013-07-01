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
%% R*-Tree
%% @doc Implementation of R* tree data structure.

-module(erstar).

-export([
    new/1,
    new/2,
    new/4,
    instance/1,
    insert/3,
    insert/2,
    remove/3,
    remove/2,
    clear/1,
    fold/3,
    foldwide/3,
    walk/2,
    walkfold/3,
    leaves/1,
    size/1,
    at/3,
    around/4,
    inbetween/5,
    locate/2,
    locate/3
]).

%%

-type treeleaf() :: {erstar_bound:bound(), any()}.
-type treenode() :: {node, empty | erstar_bound:bound(), [treenode() | treeleaf()]}.

-type rtree() ::
    {?MODULE, {pos_integer(), pos_integer(), pos_integer(), non_neg_integer()}, treenode()}.

-export_type([rtree/0, treeleaf/0, treenode/0]).

%%

-ifdef(NIFEXT).

-on_load(init/0).

init() ->
    PrivDir = case code:priv_dir(?MODULE) of
        {error, _} ->
            EbinDir = filename:dirname(code:which(?MODULE)),
            AppPath = filename:dirname(EbinDir),
            filename:join(AppPath, "priv");
        Path ->
            Path
    end,
    erlang:load_nif(filename:join(PrivDir, ?MODULE_STRING), 0).

-endif.

%%
%% @doc Creates an empty R* tree with specific maximum node capacity.
%% Same as to create a tree with minimum node capacity equal to 40% of the
%% `MaxCapacity'.
%% @see new/2

-spec new(MaxCapacity :: pos_integer()) -> rtree().

new(MaxCap) ->
    new(trunc(0.4 * MaxCap), MaxCap).

%%
%% @doc Creates an empty R* tree with specific minimum and maximum node capacity.
%% Same as to create a tree with given node capacities, choose-subtree cutout of 32
%% and reinserts count equal to 30% of the `MaxCapacity'.
%% @see new/4

-spec new(MinCapacity :: pos_integer(), MaxCapacity :: pos_integer()) -> rtree().

new(MinCap, MaxCap) ->
    new(MinCap, MaxCap, 32, trunc(0.3 * MaxCap)).

%%
%% @doc Creates an empty R* tree with specific node capacities, choose-subtree cutout
%% value and reinserts count.
%%
%% Node capacities dictate how many children each node should contain. Only the root
%% node allowed to contain lesser than `MinCapacity' number of children.
%% Minimum node capacity should be at least 2 and maximum capacity should be at
%% least double the minimum one. Practically, in the most of cases wide nodes
%% are preferred, starting from 16 or 32, and wider.
%%
%% Choose-subtree cutout value says how many children at each level are being
%% considered during each insertion. Generally, the lesser this value is, the
%% faster each insert will be, but later lookups may degrade in performance
%% instead.
%%
%% Finally, reinserts count states how many nodes should be inserted again during
%% each node split. Theretically, greater values would give better R* tree
%% lookup performance in the long term. But on the other hand they will incur
%% notable insert performace penalty. However, author's observations show that
%% there was no notable improvement in the tree quality on different, even pretty big,
%% datasets when recommended values were used.
%%
%% As paper on the subject states, the general optimal value for the minimum node
%% capacity is 40% of the maximum one, for the choose-subtree cutout is 32 and for
%% the reinserts count is at 30% of the maximum node capacity.

-spec new(MinCapacity, MaxCapacity, pos_integer(), non_neg_integer()) -> rtree() when
    MinCapacity :: pos_integer(),
    MaxCapacity :: pos_integer().

new(MinCap, MaxCap, ChooseCutout, ReinsertCount) when
    is_integer(MinCap), MinCap > 1,
    is_integer(MaxCap), MaxCap >= 2 * MinCap,
    is_integer(ChooseCutout), ChooseCutout > 0,
    is_integer(ReinsertCount), ReinsertCount >= 0, ReinsertCount < MaxCap ->
    {?MODULE, {MinCap, MaxCap, ChooseCutout, ReinsertCount}, newnode()}.

%%
%% @doc Verifies that the given term is an R* tree.

-spec instance(any()) -> boolean().

instance({?MODULE, {_, _, _, _}, {node, _, _}}) ->
    true;

instance(_) ->
    false.

%%
%% @doc Inserts new leaf into an R* tree, given its bound and arbitrary term
%% to associate with it.
%%
%% Generally, it is expensive operation. If you expect large number of inserts
%% with your usage plan, consider lowering reinserts count or maximum node
%% capacity, or both.

-spec insert(erstar_bound:bound(), any(), rtree()) -> rtree().

insert(Bound, Data, RStar = {?MODULE, Params, Root0}) ->
    Root = insert_leaf(newleaf(Bound, Data), Root0, 1, Params),
    update_root(RStar, Root, Params).

%%
%% @doc Inserts a bulk of leafs simultaneously.
%% Each leaf is a tuple containing its bound and arbitrary term.
%% @see insert/3

-spec insert([treeleaf()], rtree()) -> rtree().

insert(Leafs, RStar = {?MODULE, Params, Root0}) ->
    Root = insert_bulk(Leafs, Root0, Params),
    update_root(RStar, Root, Params).

%%
%% @doc Removes the leaf from an R* tree, given its bound and arbitrary term.
%% Does nothing if tree does not contain such leaf.

-spec remove(erstar_bound:bound(), any(), rtree()) -> rtree().

remove(Bound, Data, RStar = {?MODULE, Params, Root0}) ->
    {[Root], Orphans} = remove_leaf(newleaf(Bound, Data), Root0, true, {[], []}, Params),
    FinalRoot = insert_bulk(Orphans, Root, Params),
    update_root(RStar, FinalRoot, Params).

%%
%% @doc Removes the bulk of leaves with a single call.
%% @see remove/3

-spec remove([treeleaf()], rtree()) -> rtree().

remove(Leaves, RStar = {?MODULE, Params, Root0}) ->
    Root = remove_bulk(Leaves, Root0, [], Params),
    update_root(RStar, Root, Params).

%%
%% @doc Removes all leaves from a tree.

-spec clear(rtree()) -> rtree().

clear({?MODULE, Params, _Root}) ->
    {?MODULE, Params, newnode()}.

%%
%% @doc Folds over entire tree in a depth-first traversal.
%% Accumulates result with each call to the user-defined function. Each call is
%% given entry type, `node' or `leaf', its bound, the thing it contain, numeric
%% level in a tree and the accumulator.
%% Leaves contain arbitrary terms passed to `insert' while nodes contain list of
%% their children.

-spec fold(FoldFun, Acc, rtree()) -> Acc when
    FoldFun :: fun((node | leaf, erstar_bound:bound(), any(), pos_integer(), Acc) -> Acc),
    Acc :: any().

fold(Fun, Acc, {?MODULE, _, Root}) ->
    fold_deep(Fun, Acc, 1, Root).

%%
%% @doc Folds over entire tree in a breadth-first traversal.
%% Otherwise, the same as `fold/3'.
%% @see fold/3

-spec foldwide(FoldFun, Acc, rtree()) -> Acc when
    FoldFun :: fun((node | leaf, erstar_bound:bound(), any(), pos_integer(), Acc) -> Acc),
    Acc :: any().

foldwide(Fun, Acc, {?MODULE, _, Root}) ->
    fold_wide(Fun, Acc, 1, Root).

%%
%% @doc Walks over nodes and leaves in a tree, effectively gathering list of
%% leaves accepted by the user-defined function.
%% User-defined function should decide, given entry type and its bound, if a node
%% should be visited, should be walked over as a whole (as if to decide to walk over
%% any descendants of this node unconditionally), or a leaf should appear in
%% the result.
%% If you want to issue some specific locate query on a tree, start here.

-spec walk(WalkFun, rtree()) -> [treeleaf()] when
    WalkFun :: fun((node | leaf, erstar_bound:bound()) -> false | true | true_for_all).

walk(_WalkFun, {?MODULE, _, {_, _, []}}) ->
    [];

walk(WalkFun, {?MODULE, _, Root}) ->
    walk_node(WalkFun, [], Root).

%%
%% @doc Walks over nodes and leaves in a tree, simultaneously folding over these
%% which were accepted by the user-defined function.
%% User-defined function should decide, given entry type, its bound, thing it contains,
%% its level in a tree and the accumulator, what it wants to do further. If `{ok, Acc}'
%% is returned, the walk operation continues over other siblings in a tree. On the other
%% hand, if `{descend, Acc}' is returned, operation continues with all children of
%% the current node. It is not allowed to `descend' when walking over a leaf. Finally,
%% `{done, Acc}' ends the walk operation instantly, returning `Acc' as the final result.
%%
%% This operation is a hybrid of `fold/3' and `walk/2'.
%% @see fold/3
%% @see walk/2

-spec walkfold(WalkFun, Acc, rtree()) -> Acc when
    WalkFun :: fun((node | leaf, erstar_bound:bound(), any(), pos_integer(), Acc) -> Result),
    Result :: {ok, Acc} | {descend, Acc} | {done, Acc},
    Acc :: any().

walkfold(_WalkFun, Acc, {?MODULE, _, {_, _, []}}) ->
    Acc;

walkfold(WalkFun, Acc, {?MODULE, _, Root}) ->
    catch walk_fold(WalkFun, Acc, 1, Root).

%%
%% @doc Gathers plain list of all leaves in an R* tree.

-spec leaves(rtree()) -> [treeleaf()].

leaves(RTree) ->
    walk(fun always_true/2, RTree).

%%
%% @doc Computes the size of an R* tree.
%%
%% Please note that the computation involves traversal
%% of the whole tree.

-spec size(rtree()) -> non_neg_integer().

size(RTree) ->
    walkfold(fun count_leaves/5, 0, RTree).

%%
%% @doc Locates all the leaves which contain the given point in its bound.
%% @see locate/3

-spec at(number(), number(), rtree()) -> [treeleaf()].

at(X, Y, RStar) ->
    locate(intersect, erstar_bound:new(X, Y), RStar).

%%
%% @doc Locates all the leaves which are closer than `CloserThan' units to
%% the given point.
%% Distance to a bound is said to be the distance to its center.

-spec around(number(), number(), number(), rtree()) -> [treeleaf()].

around(X, Y, CloserThan, RStar) ->
    Dist2 = CloserThan * CloserThan,
    walk(fun (Type, Bound) -> closer_than(Type, Bound, X, Y, CloserThan, Dist2) end, RStar).

%%
%% @doc Locates all the leaves which are in between `FartherThan' and `ButCloserThan'
%% units far from the given point.
%% Distance to a bound is said to be the distance to its center.

-spec inbetween(number(), number(), number(), number(), rtree()) -> [treeleaf()].

inbetween(X, Y, FartherThan, ButCloserThan, RStar) ->
    FT2 = FartherThan * FartherThan,
    CT2 = ButCloserThan * ButCloserThan,
    WalkFun = fun (Type, Bound) -> in_between(Type, Bound, X, Y, FT2, ButCloserThan, CT2) end,
    walk(WalkFun, RStar).

%%
%% @doc Locates all the leaves getting inside the given bound.
%% @see locate/3

-spec locate(erstar_bound:bound(), rtree()) -> [treeleaf()].

locate(Where, RStar) ->
    locate(enclose, Where, RStar).

%%
%% @doc Locates all the leaves enclosed inside or overlapping the given bound.
%% Thus, `enclose' and `intersect' specify which set will be returned, respectively.

-spec locate(enclose | intersect, erstar_bound:bound(), rtree()) -> [treeleaf()].

locate(enclose, Where, RStar) ->
    walk(fun (Type, Bound) -> does_enclose(Type, Bound, Where) end, RStar);

locate(intersect, Where, RStar) ->
    walk(fun (Type, Bound) -> does_intersect(Type, Bound, Where) end, RStar).

%%

does_enclose(node, Bound, Location) ->
    erstar_bound:intersect(Location, Bound) =/= empty;

does_enclose(leaf, Bound, Location) ->
    erstar_bound:unify(Location, Bound) =:= Location.

does_intersect(_Any, Bound, Location) ->
    erstar_bound:intersect(Location, Bound) =/= empty.

closer_than(node, {X1, Y1, X2, Y2}, RX, RY, Dist, Dist2) ->
    HW = (X2 - X1) / 2,
    HH = (Y2 - Y1) / 2,
    DX = abs(X1 + HW - RX),
    DY = abs(Y1 + HH - RY),
    FX = DX + HW,
    FY = DY + HH,
    if
        DX > HW + Dist ->
            false;
        DY > HH + Dist ->
            false;
        FX * FX + FY * FY =< Dist2 ->
            true_for_all;
        DX =< HW ->
            true;
        DY =< HH ->
            true;
        true ->
            DCX = DX - HW,
            DCY = DY - HH,
            DC2 = DCX * DCX + DCY * DCY,
            DC2 =< Dist2
    end;

closer_than(leaf, Bound, RX, RY, _, Dist2) ->
    {CX, CY} = erstar_bound:center(Bound),
    DX = CX - RX,
    DY = CY - RY,
    (DX * DX + DY * DY) =< Dist2.

in_between(node, {X1, Y1, X2, Y2}, RX, RY, FT2, CT, CT2) ->
    HW = (X2 - X1) / 2,
    HH = (Y2 - Y1) / 2,
    DX = abs(X1 + HW - RX),
    DY = abs(Y1 + HH - RY),
    FX = DX + HW,
    FY = DY + HH,
    CX = max(DX - HW, 0),
    CY = max(DY - HH, 0),
    FD2 = FX * FX + FY * FY,
    if
        FD2 < FT2 ->
            false;
        CX > CT ->
            false;
        CY > CT ->
            false;
        FD2 =< CT2 ->
            if
                CX * CX + CY * CY >= FT2 ->
                    true_for_all;
                true ->
                    true
            end;
        CX =< 0 ->
            true;
        CY =< 0 ->
            true;
        true ->
            CD2 = CX * CX + CY * CY,
            CD2 =< CT2
    end;

in_between(leaf, Bound, RX, RY, FT2, _, CT2) ->
    {CX, CY} = erstar_bound:center(Bound),
    DX = CX - RX,
    DY = CY - RY,
    D2 = DX * DX + DY * DY,
    D2 > FT2 andalso D2 =< CT2.

always_true(_, _) ->
    true.

%%

maybe_reroot({reinsert, Nodes, Root}, Params) ->
    insert_bulk(Nodes, Root, setelement(4, Params, 0));

maybe_reroot(Roots, _Params) when is_list(Roots) ->
    newnode(Roots);

maybe_reroot(Root, _Params) ->
    Root.

update_root(RStar, Root, Params) ->
    setelement(3, RStar, maybe_reroot(Root, Params)).

%%

insert_bulk([], Root, _Params) ->
    Root;

insert_bulk([Leaf | Rest], Root, Params) ->
    insert_bulk(Rest, maybe_reroot(insert_leaf(Leaf, Root, 1, Params), Params), Params).

insert_leaf(Leaf, Node0, Level, Params) ->
    case has_leaves(Node0) of
        true ->
            insert_child(Leaf, Node0, Level, Params);
        _False ->
            {SubNode0, Node} = pick_subnode(bound(Leaf), Node0, Params),
            SubNode = insert_leaf(Leaf, SubNode0, Level + 1, Params),
            insert_child(SubNode, Node, Level, Params)
    end.

pick_subnode(Against, {_, Bound, Children}, Params) ->
    Picked = pick_node(Against, Children, Params),
    {Picked, newnode(Bound, Children -- [Picked])}.

pick_node(Bound, Nodes = [Node | _], Params) ->
    pick_node(has_leaves(Node), Bound, Nodes, Params).

pick_node(true, Bound, Nodes, {_, _, ChooseCutout, _}) when length(Nodes) > ChooseCutout ->
    Compare = fun (N1, N2) -> compute_area_d(Bound, N1) < compute_area_d(Bound, N2) end,
    SortedNodes = lists:sort(Compare, Nodes),
    {ClosestNodes, _} = take_part([], SortedNodes, ChooseCutout),
    needs_least(overlap, Bound, ClosestNodes);

pick_node(true, Bound, Nodes, _Params) ->
    needs_least(overlap, Bound, Nodes);

pick_node(_False, Bound, Nodes, _Params) ->
    needs_least(area, Bound, Nodes).

insert_child({reinsert, What, SubNode}, Node, Level, Params) ->
    {reinsert, What, insert_child(SubNode, Node, Level, Params)};

insert_child(List, Node, Level, Params) when is_list(List) ->
    insert_children(List, Node, Level, Params);

insert_child(Child, {_, BoundWas, Children}, Level, Params = {_, MaxCap, _, _}) when length(Children) + 1 > MaxCap ->
    split_nodes([Child], BoundWas, Children, Level, Params);

insert_child(Child, {_, Bound, Children}, _Level, _Params) ->
    newnode(erstar_bound:unify(Bound, bound(Child)), [Child | Children]).

insert_children(Cs, {_, BoundWas, CsWere}, Level, Params = {_, MaxCap, _, _}) when length(Cs) + length(CsWere) > MaxCap ->
    split_nodes(Cs, BoundWas, CsWere, Level, Params);

insert_children(Cs, {_, Bound, CsWere}, _Level, _Params) ->
    NowBound = unify_bounds(Cs, Bound),
    newnode(NowBound, Cs ++ CsWere).

unify_bounds([], Bound) ->
    Bound;

unify_bounds([Node | Rest], Bound) ->
    unify_bounds(Rest, erstar_bound:unify(Bound, bound(Node))).

split_nodes(Extra, BoundWas, Nodes, Level, {_, _, _, ReinsertCount}) when Level > 1, ReinsertCount > 0 ->
    Bound = unify_bounds(Extra, BoundWas),
    BoundCenter = erstar_bound:center(Bound),
    SortedNodes = lists:sort(fun (N1, N2) -> compare_distance(N1, N2, BoundCenter) end, Extra ++ Nodes),
    {DistantNodes, Rest} = take_part([], SortedNodes, ReinsertCount),
    {reinsert, DistantNodes, newnode(Rest)};

split_nodes(Extra, _Bound, Nodes, _Level, {MinCap, _, _, _}) ->
    {NodesByLower, NodesByUpper} = choose_axis(Extra ++ Nodes, MinCap),
    Result0 = choose_split_axis(NodesByLower, MinCap),
    Result1 = choose_split_axis(NodesByUpper, MinCap),
    split_nodes_by_distrib(Result0, Result1).

compare_distance(N1, N2, Center) ->
    C1 = erstar_bound:center(bound(N1)),
    C2 = erstar_bound:center(bound(N2)),
    distance2(C1, Center) > distance2(C2, Center).

distance2({X1, Y1}, {X2, Y2}) ->
    DX = X1 - X2,
    DY = Y1 - Y2,
    DX * DX + DY * DY.

choose_split_axis(Nodes, MinCap) ->
    G = get_distrib_goodness(overlap, Nodes, MinCap),
    choose_split_axis(Nodes, MinCap + 1, length(Nodes) - MinCap + 1, MinCap, G).

choose_split_axis(Nodes, M, M, Min, {G, B1, B2}) ->
    {G, Nodes, Min, B1, B2};

choose_split_axis(Nodes, N, M, MinSoFar, GSoFar = {ValueSoFar, _, _}) ->
    G = {Value, _, _} = get_distrib_goodness(overlap, Nodes, N),
    if
        Value < ValueSoFar ->
            choose_split_axis(Nodes, N + 1, M, N, G);
        true ->
            choose_split_axis(Nodes, N + 1, M, MinSoFar, GSoFar)
    end.

split_nodes_by_distrib({G0, Nodes, N, B1, B2}, {G1, _, _, _, _}) when G0 < G1 ->
    {Part, Rest} = take_part([], Nodes, N),
    [newnode(B1, Part), newnode(B2, Rest)];

split_nodes_by_distrib(_, {_, Nodes, N, B1, B2}) ->
    {Part, Rest} = take_part([], Nodes, N),
    [newnode(B1, Part), newnode(B2, Rest)].

choose_axis(Nodes, MinCap) ->
    {XGoodness, XNodesSorted} = get_axis_goodness(x, Nodes, MinCap),
    {YGoodness, YNodesSorted} = get_axis_goodness(y, Nodes, MinCap),
    if
        XGoodness < YGoodness ->
            XNodesSorted;
        true ->
            YNodesSorted
    end.

get_axis_goodness(Ax, Nodes, MinCap) ->
    Limit = length(Nodes) - MinCap + 1,
    ByLower = lists:sort(fun (N1, N2) -> compare_bound_l(Ax, bound(N1), bound(N2)) end, Nodes),
    ByUpper = lists:sort(fun (N1, N2) -> compare_bound_u(Ax, bound(N1), bound(N2)) end, Nodes),
    G0 = sum_distribs_goodness(ByLower, MinCap, Limit, 0),
    GR = sum_distribs_goodness(ByUpper, MinCap, Limit, G0),
    {GR, {ByLower, ByUpper}}.

compare_bound_l(x, B1, B2) ->
    erstar_bound:x1(B1) < erstar_bound:x1(B2);

compare_bound_l(y, B1, B2) ->
    erstar_bound:y1(B1) < erstar_bound:y1(B2).

compare_bound_u(x, B1, B2) ->
    erstar_bound:x2(B1) < erstar_bound:x2(B2);

compare_bound_u(y, B1, B2) ->
    erstar_bound:y2(B1) < erstar_bound:y2(B2).

sum_distribs_goodness(_Nodes, M, M, Acc) ->
    Acc;

sum_distribs_goodness(Nodes, N, M, Acc) ->
    {G, _, _} = get_distrib_goodness(margin, Nodes, N),
    sum_distribs_goodness(Nodes, N + 1, M, Acc + G).

get_distrib_goodness(What, [Node | Rest], N) ->
    get_distrib_goodness(What, Rest, N - 1, bound(Node), erstar_bound:empty()).

get_distrib_goodness(What, [], _, B1, B2) ->
    {compute_goodness(What, B1, B2), B1, B2};

get_distrib_goodness(What, [Node | Rest], 0, B1, B2) ->
    get_distrib_goodness(What, Rest, 0, B1, erstar_bound:unify(B2, bound(Node)));

get_distrib_goodness(What, [Node | Rest], N, B1, B2) ->
    get_distrib_goodness(What, Rest, N - 1, erstar_bound:unify(B1, bound(Node)), B2).

compute_goodness(margin, B1, B2) ->
    erstar_bound:margin(B1) + erstar_bound:margin(B2);

compute_goodness(overlap, B1, B2) ->
    erstar_bound:overlap(B1, B2).

take_part(Part, Rest, 0) ->
    {Part, Rest};

take_part(Part, [Node | Rest], N) ->
    take_part([Node | Part], Rest, N - 1).

%%

needs_least(Criteria, Bound, Nodes = [Node | Rest]) ->
    needs_least(Criteria, Nodes, compute_criteria(Criteria, Bound, Node, Nodes), Node, Bound, Rest).

needs_least(_, _Nodes, _Least, Node, _Bound, []) ->
    Node;

needs_least(Criteria, Nodes, LeastSoFar, NodeSoFar, Bound, [Node | Rest]) ->
    case compute_criteria(Criteria, Bound, Node, Nodes) of
        Least when Least < LeastSoFar ->
            needs_least(Criteria, Nodes, Least, Node, Bound, Rest);
        _ ->
            needs_least(Criteria, Nodes, LeastSoFar, NodeSoFar, Bound, Rest)
    end.

compute_criteria(area, Bound, Node, _Nodes) ->
    compute_area_d(Bound, Node);

compute_criteria(overlap, Bound, Node, Nodes) ->
    NodeBound = bound(Node),
    OverlapWas = total_overlap(0, NodeBound, Nodes),
    NowBound = erstar_bound:unify(NodeBound, Bound),
    OverlapNow = total_overlap(0, NowBound, Nodes),
    OverlapNow - OverlapWas.

total_overlap(Acc, _Bound, []) ->
    Acc;

total_overlap(Acc, Bound, [{_, NodeBound, _} | Rest]) ->
    total_overlap(Acc + erstar_bound:overlap(Bound, NodeBound), Bound, Rest).

compute_area_d(Bound, Node) ->
    NodeBound = bound(Node),
    erstar_bound:area(erstar_bound:unify(NodeBound, Bound)) - erstar_bound:area(NodeBound).

%%

remove_bulk([], Root, Orphans, Params) ->
    insert_bulk(Orphans, Root, Params);

remove_bulk([Leaf | Rest], WasRoot, WasOrphans, Params) ->
    {[Root], Orphans} = remove_leaf(Leaf, WasRoot, true, {[], WasOrphans -- [Leaf]}, Params),
    remove_bulk(Rest, Root, Orphans, Params).

remove_leaf(Leaf, Node = {_, _, Children}, IsRoot, {Nodes, Orphans0}, Params) ->
    LeafBound = bound(Leaf),
    NodeBound = bound(Node),
    case erstar_bound:unify(LeafBound, NodeBound) of
        NodeBound ->
            SubAcc = {[], Orphans0},
            HasLeaves = has_leaves(Node),
            {LeftNodes, Orphans} = remove_leaf(Leaf, Children, HasLeaves, IsRoot, SubAcc, Params),
            {LeftNodes ++ Nodes, Orphans};
        _ ->
            {[Node | Nodes], Orphans0}
    end.

remove_leaf(_Leaf, [], _, false, {Acc, Orphans}, {MinCap, _, _, _}) when length(Acc) < MinCap ->
    {[], extract_leaves(Acc, Orphans)};

remove_leaf(_Leaf, [], _, true, {[], Orphans}, _Params) ->
    {[newnode()], Orphans};

remove_leaf(_Leaf, [], _, true, {Acc = [{node, _, _}], Orphans}, _Params) ->
    {Acc, Orphans};

remove_leaf(_Leaf, [], _Any, _IsRoot, {Acc, Orphans}, _Params) ->
    {[newnode(Acc)], Orphans};

remove_leaf(Leaf, Leaves, true, IsRoot, {_, Orphans}, Params) ->
    remove_leaf(Leaf, [], true, IsRoot, {Leaves -- [Leaf], Orphans}, Params);

remove_leaf(Leaf, [Node | Rest], HasLeaves, IsRoot, Acc, Params) ->
    remove_leaf(Leaf, Rest, HasLeaves, IsRoot, remove_leaf(Leaf, Node, false, Acc, Params), Params).

extract_leaves([], Acc) ->
    Acc;

extract_leaves(Leaves = [{_, _} | _], Acc) ->
    Leaves ++ Acc;

extract_leaves([Node | Rest], Acc) ->
    extract_leaves(Rest, walk_node(fun (_, _) -> true end, Acc, Node)).

%%

fold_deep(_Fun, Acc, _Level, []) ->
    Acc;

fold_deep(Fun, Acc, Level, [Node | Rest]) ->
    fold_deep(Fun, fold_deep(Fun, Acc, Level, Node), Level, Rest);

fold_deep(Fun, Acc, Level, Node = {_, _, Children}) ->
    fold_deep(Fun, fold_node(Fun, Acc, Level, Node), Level + 1, Children);

fold_deep(Fun, Acc, Level, Leaf) ->
    fold_node(Fun, Acc, Level, Leaf).

fold_wide(Fun, Acc, Level, Node = {node, _, _}) ->
    fold_wide(Fun, Acc, Level, [Node]);

fold_wide(_Fun, Acc, _Level, []) ->
    Acc;

fold_wide(Fun, Acc0, Level, Nodes = [{node, _, _} | _]) ->
    {Acc, LowerNodes} = fold_level(Fun, {Acc0, []}, Level, Nodes),
    fold_wide(Fun, Acc, Level + 1, LowerNodes);

fold_wide(Fun, Acc, Level, [Leaf | Rest]) ->
    fold_wide(Fun, fold_node(Fun, Acc, Level, Leaf), Level, Rest).

fold_level(_Fun, Acc, _Level, []) ->
    Acc;

fold_level(Fun, {Acc, Lower}, Level, [Node = {_, _, Children} | Rest]) ->
    fold_level(Fun, {fold_node(Fun, Acc, Level, Node), Lower ++ Children}, Level, Rest).

fold_node(Fun, Acc, Level, {node, Bound, Children}) ->
    Fun(node, Bound, Children, Level, Acc);

fold_node(Fun, Acc, Level, {Bound, Data}) ->
    Fun(leaf, Bound, Data, Level, Acc).

%%

count_leaves(node, _, Children = [{_, _} | _], _, Acc) ->
    {ok, Acc + length(Children)};

count_leaves(node, _, _, _, Acc) ->
    {descend, Acc}.

%%

walk_node(_Walk, Acc, []) ->
    Acc;

walk_node(Walk, Acc, [Node | Rest]) ->
    walk_node(Walk, walk_node(Walk, Acc, Node), Rest);

walk_node(Walk, Acc, Node) ->
    walk_node(Walk(nodetype(Node), bound(Node)), Walk, Acc, Node).

walk_node(true, Walk, Acc, {_, _, Children}) ->
    walk_node(Walk, Acc, Children);

walk_node(true_for_all, _Walk, Acc, {_, _, Children}) ->
    walk_leaves(Children, Acc);

walk_node(true, _Walk, Acc, Leaf) ->
    [Leaf | Acc];

walk_node(false, _Walk, Acc, _Any) ->
    Acc.

walk_leaves([], Acc) ->
    Acc;

walk_leaves([{_, _, Nodes} | Rest], Acc) ->
    walk_leaves(Rest, walk_leaves(Nodes, Acc));

walk_leaves(Leaves, Acc) ->
    Leaves ++ Acc.

%%

walk_fold(_Walk, Acc, _Level, []) ->
    Acc;

walk_fold(Walk, Acc, Level, [Node | Rest]) ->
    walk_fold(Walk, walk_fold(Walk, Acc, Level, Node), Level, Rest);

walk_fold(Walk, Acc, Level, Node) ->
    walk_decide(Walk(nodetype(Node), bound(Node), data(Node), Level, Acc), Walk, Level, Node).

walk_decide({descend, Acc}, Walk, Level, {_, _, Children}) ->
    walk_fold(Walk, Acc, Level + 1, Children);

walk_decide({ok, Acc}, _Walk, _Level, _Any) ->
    Acc;

walk_decide({done, Acc}, _Walk, _Level, _Any) ->
    throw(Acc).

%%

newnode() ->
    {node, erstar_bound:empty(), []}.

newnode(Children = [Node | Rest]) ->
    {node, unify_bounds(Rest, bound(Node)), Children}.

newnode(Bound, Children) ->
    {node, Bound, Children}.

newleaf(Bound, Data) ->
    {Bound, Data}.

nodetype({_, _, _}) ->
    node;

nodetype({_, _}) ->
    leaf.

data({_, _, Children}) ->
    Children;

data({_, Data}) ->
    Data.

bound({Bound, _}) ->
    Bound;

bound({_, Bound, _}) ->
    Bound.

has_leaves({_, _, []}) ->
    true;

has_leaves({_, _, [{_, _} | _]}) ->
    true;

has_leaves(_) ->
    false.

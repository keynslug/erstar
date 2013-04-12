%%
%% R*-Tree

-module(erstar).

-export([
    new/1,
    new/2,
    new/4,
    instance/1,
    insert/3,
    insert/2,
    fold/3,
    foldwide/3,
    walk/2,
    leaves/1,
    size/1,
    at/3,
    around/4,
    locate/2,
    locate/3
]).

%%

-type treeleaf() :: {erstar_bound:bound(), any()}.
-type treenode() :: {node, empty | erstar_bound:bound(), [treenode() | treeleaf()]}.

-type rtree() ::
    {?MODULE, {pos_integer(), pos_integer(), pos_integer(), non_neg_integer()}, treenode()}.

-export_type([rtree/0]).

%%

-spec new(pos_integer()) -> rtree().

new(MaxCap) ->
    new(trunc(0.4 * MaxCap), MaxCap).

-spec new(pos_integer(), pos_integer()) -> rtree().

new(MinCap, MaxCap) ->
    new(MinCap, MaxCap, 32, trunc(0.3 * MaxCap)).

-spec new(pos_integer(), pos_integer(), pos_integer(), pos_integer()) -> rtree().

new(MinCap, MaxCap, ChooseCutout, ReinsertCount) when
    is_integer(MinCap), MinCap > 1,
    is_integer(MaxCap), MaxCap >= 2 * MinCap,
    is_integer(ChooseCutout), ChooseCutout > 0,
    is_integer(ReinsertCount), ReinsertCount >= 0, ReinsertCount < MaxCap ->
    {?MODULE, {MinCap, MaxCap, ChooseCutout, ReinsertCount}, newnode()}.

%%

-spec instance(any()) -> boolean().

instance({?MODULE, {_, _, _, _}, {node, _, _}}) ->
    true;

instance(_) ->
    false.

%%

-spec insert(erstar_bound:bound(), any(), rtree()) -> rtree().

insert(Bound, Data, RStar = {?MODULE, Params, Root0}) ->
    Root = insert_leaf(newleaf(Bound, Data), Root0, 1, Params),
    update_root(RStar, Root, Params).

-spec insert([treeleaf()], rtree()) -> rtree().

insert(Leafs, RStar = {?MODULE, Params, Root0}) ->
    Root = insert_bulk(Leafs, Root0, Params),
    update_root(RStar, Root, Params).

%%

-spec fold(FoldFun, Acc, rtree()) -> Acc when
    FoldFun :: fun((node | leaf, erstar_bound:bound(), any(), pos_integer(), Acc) -> Acc),
    Acc :: any().

fold(Fun, Acc, {?MODULE, _, Root}) ->
    fold_deep(Fun, Acc, 1, Root).

%%

-spec foldwide(FoldFun, Acc, rtree()) -> Acc when
    FoldFun :: fun((node | leaf, erstar_bound:bound(), any(), pos_integer(), Acc) -> Acc),
    Acc :: any().

foldwide(Fun, Acc, {?MODULE, _, Root}) ->
    fold_wide(Fun, Acc, 1, Root).

%%

-spec walk(WalkFun, rtree()) -> [treeleaf()] when
    WalkFun :: fun((node | leaf, erstar_bound:bound()) -> boolean()).

walk(WalkFun, {?MODULE, _, Root}) ->
    walk_node(WalkFun, [], Root).

%%

-spec leaves(rtree()) -> [treeleaf()].

leaves(RTree) ->
    walk(fun always_true/2, RTree).

%%

-spec size(rtree()) -> non_neg_integer().

size({?MODULE, _, Root}) ->
    count_leaves(0, Root).

%%

-spec at(number(), number(), rtree()) -> [treeleaf()].

at(X, Y, RStar) ->
    locate(intersect, erstar_bound:new(X, Y), RStar).

%%

-spec around(number(), number(), number(), rtree()) -> [treeleaf()].

around(X, Y, CloserThan, RStar) ->
    Dist2 = CloserThan * CloserThan,
    walk(fun (Type, Bound) -> closer_than(Type, Bound, X, Y, CloserThan, Dist2) end, RStar).

%%

-spec locate(erstar_bound:bound(), rtree()) -> [treeleaf()].

locate(Where, RStar) ->
    locate(enclose, Where, RStar).

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
    if
        DX > HW + Dist ->
            false;
        DY > HH + Dist ->
            false;
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

count_leaves(Acc, []) ->
    Acc;

count_leaves(Acc, [Node | Rest]) ->
    count_leaves(count_leaves(Acc, Node), Rest);

count_leaves(Acc, Node) ->
    count_leaves(has_leaves(Node), Acc, Node).

count_leaves(true, Acc, {_, _, Children}) ->
    Acc + length(Children);

count_leaves(false, Acc, {_, _, Children}) ->
    count_leaves(Acc, Children).

%%

walk_node(_Walk, Acc, []) ->
    Acc;

walk_node(Walk, Acc, [Node | Rest]) ->
    walk_node(Walk, walk_node(Walk, Acc, Node), Rest);

walk_node(Walk, Acc, Node) ->
    walk_node(Walk(nodetype(Node), bound(Node)), Walk, Acc, Node).

walk_node(true, Walk, Acc, {_, _, Children}) ->
    walk_node(Walk, Acc, Children);

walk_node(true, _Walk, Acc, Leaf) ->
    [Leaf | Acc];

walk_node(false, _Walk, Acc, _Any) ->
    Acc.

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

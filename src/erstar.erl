%%
%% R*-Tree

-module(erstar).

-export([
    new/1,
    new/2,
    new/4,
    instance/1,
    insert/3,
    fold/3,
    foldwide/3
]).

%%

-type treeleaf() :: {erstar_bound:bound(), any()}.
-type treenode() :: {node, empty | erstar_bound:bound(), [treenode() | treeleaf()]}.

-type tree() :: {?MODULE, tuple(), treenode()}.

-export_type([tree/0]).

-compile([export_all]).

%%

-spec new(pos_integer()) -> tree().

new(MaxCap) ->
    new(trunc(0.4 * MaxCap), MaxCap).

-spec new(pos_integer(), pos_integer()) -> tree().

new(MinCap, MaxCap) ->
    new(MinCap, MaxCap, 32, trunc(0.3 * MaxCap)).

-spec new(pos_integer(), pos_integer(), pos_integer(), pos_integer()) -> tree().

new(MinCap, MaxCap, ChooseCutout, ReinsertCount) when
    is_integer(MinCap), MinCap > 1,
    is_integer(MaxCap), MaxCap >= 2 * MinCap,
    is_integer(ChooseCutout), ChooseCutout > 0,
    is_integer(ReinsertCount), ReinsertCount > 0, ReinsertCount < MaxCap ->
    {?MODULE, {MinCap, MaxCap, ChooseCutout, ReinsertCount}, newnode()}.

%%

-spec instance(any()) -> boolean().

instance({?MODULE, {_, _, _, _}, {node, _, _}}) ->
    true;

instance(_) ->
    false.

%%

-spec insert(erstar_bound:bound(), any(), tree()) -> tree().

insert(Bound, Data, RStar = {?MODULE, Params, Root0}) ->
    Root = insert_leaf(newleaf(Bound, Data), Root0, Params),
    update_root(RStar, Root).

%%

-spec fold(FoldFun, Acc, tree()) -> Acc when
    FoldFun :: fun((node | leaf, erstar_bound:bound(), any(), pos_integer(), Acc) -> Acc),
    Acc :: any().

fold(Fun, Acc, {?MODULE, _, Root}) ->
    fold_deep(Fun, Acc, 1, Root).

%%

-spec foldwide(FoldFun, Acc, tree()) -> Acc when
    FoldFun :: fun((node | leaf, erstar_bound:bound(), any(), pos_integer(), Acc) -> Acc),
    Acc :: any().

foldwide(Fun, Acc, {?MODULE, _, Root}) ->
    fold_wide(Fun, Acc, 1, Root).

%%

update_root(RStar, Roots) when is_list(Roots) ->
    update_root(RStar, newnode(Roots));

update_root(RStar, Root) ->
    setelement(3, RStar, Root).

%%

insert_leaf(Leaf, Node0, Params) ->
    case has_leaves(Node0) of
        true ->
            insert_child([Leaf], Node0, Params);
        _False ->
            {SubNode0, Node} = pick_subnode(bound(Leaf), Node0),
            SubNode = insert_leaf(Leaf, SubNode0, Params),
            insert_child(SubNode, Node, Params)
    end.

pick_subnode(Bound, {_, Bound, Children}) ->
    {Picked, Rest} = pick_node(Bound, Children),
    {Picked, {Bound, Rest}}.

pick_node(Bound, Nodes = [Node | _]) ->
    pick_node(has_leaves(Node), Bound, Nodes).

pick_node(true, Bound, Nodes) ->
    needs_least(overlap, Bound, Nodes);

pick_node(_False, Bound, Nodes) ->
    needs_least(area, Bound, Nodes).

insert_child(Children, {_, _, ChildrenWere}, Params = {_, MaxCap, _, _}) when length(Children) + length(ChildrenWere) > MaxCap ->
    split_nodes(Children ++ ChildrenWere, Params);

insert_child(Children, {_, Bound, ChildrenWere}, _Params) ->
    NowBound = unify_bounds(Children, Bound),
    newnode(NowBound, Children ++ ChildrenWere).

unify_bounds([], Bound) ->
    Bound;

unify_bounds([Node | Rest], Bound) ->
    unify_bounds(Rest, erstar_bound:unify(Bound, bound(Node))).

split_nodes(Nodes, _) ->
    %% dummy
    {Part, Rest} = take_part([], Nodes, length(Nodes) div 2),
    [newnode(Part), newnode(Rest)].

take_part(Part, Rest, 0) ->
    {Part, Rest};

take_part(Part, [Node | Rest], N) ->
    take_part([Node | Part], Rest, N - 1).

%%

needs_least(Criteria, Bound, Nodes = [Node | Rest]) ->
    needs_least(Criteria, Nodes, compute_criteria(Criteria, Bound, Node, Nodes), Node, Bound, Rest).

needs_least(_, Nodes, _Least, Node, _Bound, []) ->
    {Node, Nodes -- [Node]};

needs_least(Criteria, Nodes, LeastSoFar, NodeSoFar, Bound, [Node | Rest]) ->
    case compute_criteria(Criteria, Bound, Node, Nodes) of
        Least when Least < LeastSoFar ->
            needs_least(Criteria, Nodes, Least, Node, Bound, Rest);
        _ ->
            needs_least(Criteria, Nodes, LeastSoFar, NodeSoFar, Bound, Rest)
    end.

compute_criteria(area, Bound, Node, _Nodes) ->
    NodeBound = bound(Node),
    erstar_bound:area(erstar_bound:unify(NodeBound, Bound)) - erstar_bound:area(NodeBound);

compute_criteria(overlap, Bound, Node, Nodes) ->
    NodeBound = bound(Node),
    OverlapWas = lists:sum([erstar_bound:overlap(NodeBound, bound(N)) || N <- Nodes, N =/= Node]),
    NowBound = erstar_bound:unify(NodeBound, Bound),
    OverlapNow = lists:sum([erstar_bound:overlap(NowBound, bound(N)) || N <- Nodes, N =/= Node]),
    OverlapNow - OverlapWas.

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

newnode() ->
    {node, erstar_bound:empty(), []}.

newnode(Children = [Node | Rest]) ->
    {node, unify_bounds(Rest, bound(Node)), Children}.

newnode(Bound, Children) ->
    {node, Bound, Children}.

newleaf(Bound, Data) ->
    {Bound, Data}.

bound({Bound, _}) ->
    Bound;

bound({_, Bound, _}) ->
    Bound.

children({_, _, Children}) ->
    Children.

data({_, Data}) ->
    Data.

has_leaves({_, _, []}) ->
    true;

has_leaves({_, _, [{_, _} | _]}) ->
    true;

has_leaves(_) ->
    false.

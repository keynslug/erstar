%%

-module(erstar_bound_test).
-include_lib("eunit/include/eunit.hrl").

-define(M, erstar_bound).

area_test() ->
    ?assertEqual(0, ?M:area(?M:new(42, 24))),
    ?assertEqual(1, ?M:area(?M:new(42, 24, 1, 1))),
    ?assertEqual(0, ?M:area(?M:new(42, 24, 4242, 0))),
    ?assertEqual(42 * 24, ?M:area(?M:new(42, 24, 42, 24))),
    ok.

margin_test() ->
    ?assertEqual(0, ?M:margin(?M:new(42, 24))),
    ?assertEqual(2, ?M:margin(?M:new(42, 24, 1, 1))),
    ?assertEqual(4242, ?M:margin(?M:new(42, 24, 4242, 0))),
    ?assertEqual(42 + 24, ?M:margin(?M:new(42, 24, 42, 24))),
    ok.

unify_points_test() ->
    B0 = ?M:new(42, 24),
    B1 = ?M:new(16, 32),
    B = ?M:unify(B0, B1),
    ?assertEqual(B, {16, 24, 42 - 16, 32 - 24}),
    dump(unify_points_test, B0, B1, B).

unify_same_test() ->
    B0 = ?M:new(42, 24, 42, 24),
    ?assertEqual(B0, ?M:unify(B0, B0)).

unify_test() ->
    B0 = ?M:new(42, 24, 42, 24),
    B1 = ?M:new(13, 14, 15, 16),
    B = ?M:unify(B0, B1),
    ?assertEqual(B, {13, 14, 42 + 42 - 13, 48 - 14}),
    dump(unify_test, B0, B1, B).

unify_commutative_test() ->
    B = ?M:unify(?M:new(13, 14, 15, 16), ?M:new(42, 24, 42, 24)),
    ?assertEqual(B, {13, 14, 42 + 42 - 13, 48 - 14}).

intersect_points_test() ->
    B = ?M:intersect(?M:new(42, 24), ?M:new(16, 32)),
    ?assertEqual(B, empty).

intersect_same_test() ->
    B0 = ?M:new(42, 24, 42, 24),
    ?assertEqual(B0, ?M:intersect(B0, B0)).

intersect_edge_test() ->
    B0 = ?M:new(3, 1, 7, 3),
    B1 = ?M:new(4, 4, 8, 3),
    B = ?M:intersect(B0, B1),
    ?assertEqual(B, {4, 4, 6, 0}),
    dump(intersect_edge_test, B0, B1, B).

intersect_edge_commutative_test() ->
    B0 = ?M:new(4, 4, 8, 3),
    B1 = ?M:new(3, 1, 7, 3),
    ?assertEqual(?M:intersect(B0, B1), {4, 4, 6, 0}).

intersect_empty_test() ->
    B0 = ?M:new(4, 5, 4, 8),
    B1 = ?M:new(3, 1, 7, 3),
    ?assertEqual(?M:intersect(B0, B1), empty).

intersect_cross_test() ->
    B0 = ?M:new(1, 3, 6, 2),
    B1 = ?M:new(3, 1, 2, 6),
    B = ?M:intersect(B0, B1),
    ?assertEqual(B, {3, 3, 2, 2}),
    dump(intersect_cross_test, B0, B1, B).

intersect_innermost_test() ->
    B0 = ?M:new(2, 3, 6, 8),
    B1 = ?M:new(3, 6, 2, 1),
    B = ?M:intersect(B0, B1),
    ?assertEqual(B, B1),
    dump(intersect_innermost_test, B0, B1, B).

intersect_section_test() ->
    B0 = ?M:new(2, 2, 4, 6),
    B1 = ?M:new(4, 6, 8, 10),
    B = ?M:intersect(B0, B1),
    ?assertEqual(B, {4, 6, 2, 2}),
    dump(intersect_section_test, B0, B1, B).

overlap_points_test() ->
    B = ?M:overlap(?M:new(42, 24), ?M:new(16, 32)),
    ?assert(B == 0).

overlap_edge_test() ->
    B0 = ?M:new(3, 1, 7, 3),
    B1 = ?M:new(4, 4, 8, 3),
    ?assert(?M:overlap(B0, B1) == 0).

overlap_cross_test() ->
    B0 = ?M:new(1.1, 3.2, 6.3, 2.4),
    B1 = ?M:new(3.5, 1.6, 2.7, 6.8),
    ?assertEqual(?M:overlap(B0, B1), 6.48).

%%

dump(Site, B0, B1, BR) ->
    file:write_file(
        atom_to_list(Site) ++ ".svg",
        erstar_svg:render([{bound, mag(B0), 1}, {bound, mag(B1), 2}, {bound, mag(BR), 3}])
    ).

mag(B) ->
    {X, Y} = erstar_bound:lowerleft(B),
    {W, H} = erstar_bound:dimensions(B),
    erstar_bound:new(X * 10, Y * 10, W * 10, H * 10).

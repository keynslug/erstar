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
%% @doc A several features to ease using erstar as a geospatial data structure.
%%
%% Assumes that any coordinates given are geospatial, i.e. in two-dimensional spherical
%% coordinate system, expressed in radians. Any distances are spherical angles of great
%% circle arcs, again expressed in radians.
%%
%% Please note, that computations performed are far more complex than these in general
%% purpose queries.

-module(erstar_geo).

-export([
    around/4,
    inbetween/5
]).

-export([
    deg_to_rad/1,
    rad_to_deg/1
]).

%%

%% @doc Locates all the leaves which are closer than `CloserThan' radians to
%% the given point on a sphere.
%% Distance is measured along the shortest path, which is an arc of great circle
%% connecting two points.
%% Distance to a bound is said to be the distance to its midpoint.

-spec around(number(), number(), number(), erstar:rtree()) -> [erstar:treeleaf()].

around(Phi, Lambda, CloserThan, RStar) ->
    erstar:walk(fun (Type, Bound) ->
        closer_than(Type, Bound, Phi, Lambda, CloserThan)
    end, RStar).

%% @doc Locates all the leaves which are in between `FartherThan` and `CloserThan'
%% radians far from the given point on a sphere.
%% Distance is measured along the shortest path, which is an arc of great circle
%% connecting two points.
%% Distance to a bound is said to be the distance to its midpoint.

-spec inbetween(number(), number(), number(), number(), erstar:rtree()) -> [erstar:treeleaf()].

inbetween(Phi, Lambda, FartherThan, CloserThan, RStar) ->
    erstar:walk(fun (Type, Bound) ->
        in_between(Type, Bound, Phi, Lambda, FartherThan, CloserThan)
    end, RStar).

%%

-define(PI, 3.141592653589793).

%% @doc Converts degrees to radians.

-spec deg_to_rad(number()) -> float().

deg_to_rad(A) ->
    A * ?PI / 180.0.

%% @doc Converts radians to degrees.

-spec rad_to_deg(number()) -> float().

rad_to_deg(A) ->
    A * 180.0 / ?PI.

%%

closer_than(node, Bound, RPhi, RLambda, Dist) ->
    is_point_inside(RPhi, RLambda, Bound) orelse
        has_small_circle_extent_overlap(RPhi, RLambda, Dist, Bound);

closer_than(leaf, Bound, RPhi, RLambda, Dist) ->
    {CPhi, CLambda} = erstar_bound:center(Bound),
    distance(CPhi, CLambda, RPhi, RLambda) =< Dist.

in_between(node, Bound, RPhi, RLambda, FDist, CDist) ->
    closer_than(node, Bound, RPhi, RLambda, CDist) andalso
        not is_inside_small_circle(RPhi, RLambda, FDist, Bound);

in_between(leaf, Bound, RPhi, RLambda, FDist, CDist) ->
    {CPhi, CLambda} = erstar_bound:center(Bound),
    Dist = distance(CPhi, CLambda, RPhi, RLambda),
    Dist =< CDist andalso Dist >= FDist.

%%

distance(Phi1, Lambda1, Phi2, Lambda2) ->
    DLambda = Lambda2 - Lambda1,
    SinPhi1 = sin(Phi1),
    CosPhi1 = cos(Phi1),
    SinPhi2 = sin(Phi2),
    CosPhi2 = cos(Phi2),
    CosDLambda = cos(DLambda),
    math:atan2(
        math:sqrt(
            math:pow(CosPhi2 * sin(DLambda), 2.0) +
            math:pow(CosPhi1 * SinPhi2 - SinPhi1 * CosPhi2 * CosDLambda, 2.0)
        ),
        SinPhi1 * SinPhi2 + CosPhi1 * CosPhi2 * CosDLambda
    ).

small_circle_lambda_by_phi(RPhi, RLambda, Dist, Phi) ->
    case cos(RPhi) * cos(Phi) of
        0.0 ->
            undefined;
        CosMult ->
            case (cos(Dist) - sin(RPhi) * sin(Phi)) / CosMult of
                1.0 ->
                    RLambda;
                -1.0 ->
                    RLambda;
                CosDiv when CosDiv > 1 ->
                    undefined;
                CosDiv when CosDiv < -1 ->
                    undefined;
                CosDiv ->
                    Lambda = math:acos(CosDiv),
                    {RLambda - Lambda, RLambda + Lambda}
            end
    end.

small_circle_phi_by_lambda(RPhi, RLambda, Dist, Lambda) ->
    SinRPhi = sin(RPhi),
    CosRPhiDLambda = cos(RPhi) * cos(Lambda - RLambda),
    C = math:sqrt(SinRPhi * SinRPhi + CosRPhiDLambda * CosRPhiDLambda),
    Gamma = math:acos(SinRPhi / C),
    case cos(Dist) / C of
        Cos when Cos > 1.0 ->
            undefined;
        Cos when Cos < -1.0 ->
            undefined;
        Cos ->
            SubPhi = math:asin(Cos),
            case {SubPhi - Gamma, ?PI - SubPhi - Gamma} of
                {R, R} ->
                    R;
                R ->
                    R
            end
    end.

small_circle_extents(RPhi, _RLambda, Dist) when Dist >= ?PI / 2 ->
    Phi1 = RPhi - Dist,
    Phi2 = RPhi + Dist,
    {Phi1, -?PI, Phi2, +?PI};

small_circle_extents(RPhi, RLambda, Dist) ->
    Phi1 = RPhi - Dist,
    Phi2 = RPhi + Dist,
    case sin(RPhi) / cos(Dist) of
        S when S >= 1.0 ->
            {Phi1, -?PI, Phi2, +?PI};
        S when S =< -1.0 ->
            {Phi1, -?PI, Phi2, +?PI};
        S ->
            PhiM = math:asin(S),
            {Lambda1, Lambda2} = small_circle_lambda_by_phi(RPhi, RLambda, Dist, PhiM),
            {Phi1, Lambda1, Phi2, Lambda2}
    end.

%%

is_point_inside(Phi, Lambda, {Phi1, Lambda1, Phi2, Lambda2}) ->
    Phi >= Phi1 andalso Phi =< Phi2 andalso
        Lambda >= Lambda1 andalso Lambda =< Lambda2.

is_inside_small_circle(Phi, Lambda, Dist, {Phi1, Lambda1, Phi2, Lambda2}) ->
    distance(Phi, Lambda, Phi1, Lambda1) =< Dist andalso
        distance(Phi, Lambda, Phi1, Lambda2) =< Dist andalso
        distance(Phi, Lambda, Phi2, Lambda1) =< Dist andalso
        distance(Phi, Lambda, Phi2, Lambda2) =< Dist.

has_small_circle_extent_overlap(RPhi, RLambda, Dist, Bound) ->
    Extents = small_circle_extents(RPhi, RLambda, Dist),
    erstar_bound:intersect(Extents, Bound) =/= empty.

has_small_circle_overlap(RPhi, RLambda, Dist, {Phi1, Lambda1, Phi2, Lambda2}) ->
    has_small_circle_overlap(
        small_circle_lambda_by_phi(RPhi, RLambda, Dist, Phi1), Lambda1, Lambda2
    ) orelse
        has_small_circle_overlap(
            small_circle_lambda_by_phi(RPhi, RLambda, Dist, Phi2), Lambda1, Lambda2
        ) orelse
        has_small_circle_overlap(
            small_circle_phi_by_lambda(RPhi, RLambda, Dist, Lambda1), Phi1, Phi2
        ) orelse
        has_small_circle_overlap(
            small_circle_phi_by_lambda(RPhi, RLambda, Dist, Lambda2), Phi1, Phi2
        ).

has_small_circle_overlap(undefined, _, _) ->
    false;

has_small_circle_overlap({B1, B2}, A1, A2) ->
    max(A1, B1) < min(A2, B2);

has_small_circle_overlap(B, A1, A2) ->
    A1 =< B andalso B =< A2.

%%

sin(X) ->
    math:sin(X).

cos(X) ->
    math:cos(X).

/**
 * Copyright (c) 2013 Andrew Majorov <encube.ul@gmail.com>
 *
 * All Rights Reserved.
 *
 * Redistribution and use in source and binary forms, with or without modification,
 * are permitted provided that the following conditions are met:
 *
 * 1. Redistributions of source code must retain the above copyright notice, this
 *    list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright notice,
 *    this list of conditions and the following disclaimer in the documentation
 *    and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS ``AS IS'' AND ANY
 * EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE LIABLE FOR ANY
 * DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
 * ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

#define _GNU_SOURCE

#include "erl_nif.h"
#include "math.h"

#include "erstar_geo_math.h"

//
// Module-wide activities

static ERL_NIF_TERM NODE;
static ERL_NIF_TERM LEAF;
static ERL_NIF_TERM E_TRUE;
static ERL_NIF_TERM E_FALSE;
static ERL_NIF_TERM UNDEFINED;

inline ERL_NIF_TERM atom(ErlNifEnv * env, const char * name);

static int on_load(ErlNifEnv * env, void ** priv, ERL_NIF_TERM info) {
    NODE = atom(env, "node");
    LEAF = atom(env, "leaf");
    E_TRUE = atom(env, "true");
    E_FALSE = atom(env, "false");
    UNDEFINED = atom(env, "undefined");
    return 0;
}

static int on_upgrade(ErlNifEnv * env, void ** priv, void ** old_priv, ERL_NIF_TERM info) {
    return on_load(env, priv, info);
}

static void on_unload(ErlNifEnv * env, void * priv) {
}

//

inline int get_coerce_double(ErlNifEnv * env, ERL_NIF_TERM arg, double * dp) {
    ErlNifSInt64 i64;
    return enif_get_double(env, arg, dp) || (
        enif_get_int64(env, arg, &i64) && (*dp = (double)(i64), 1)
    );
}

inline ERL_NIF_TERM atom(ErlNifEnv * env, const char * name) {
    ERL_NIF_TERM ret;
    if (enif_make_existing_atom(env, name, &ret, ERL_NIF_LATIN1)) {
        return ret;
    }
    return enif_make_atom(env, name);
}

//
// Native implementations

#define FAIL_ON_ERROR(e) \
    if (!(e)) { return enif_make_badarg(env); }

#define MIN(a, b) \
    ((a) < (b) ? (a) : (b))

#define MAX(a, b) \
    ((a) < (b) ? (b) : (a))

#define INTERSECTS(a1, b1, a2, b2) \
    MAX(a1, a2) <= MIN(b1, b2)

static ERL_NIF_TERM _distance(ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[]) {

    double phi1, lambda1, phi2, lambda2;

    FAIL_ON_ERROR(argc == 4);
    FAIL_ON_ERROR(get_coerce_double(env, argv[0], &phi1));
    FAIL_ON_ERROR(get_coerce_double(env, argv[1], &lambda1));
    FAIL_ON_ERROR(get_coerce_double(env, argv[2], &phi2));
    FAIL_ON_ERROR(get_coerce_double(env, argv[3], &lambda2));

    return enif_make_double(env, distance(phi1, lambda1, phi2, lambda2));

}

static ERL_NIF_TERM _closer_than(ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[]) {

    int bound_arity;
    const ERL_NIF_TERM * bound;

    double phi, lambda, d;
    double phi1, lambda1, phi2, lambda2;
    double i_phi1, i_lambda1, i_phi2, i_lambda2;

    FAIL_ON_ERROR(argc == 5);
    FAIL_ON_ERROR(enif_get_tuple(env, argv[1], &bound_arity, &bound));
    FAIL_ON_ERROR(bound_arity == 4);
    FAIL_ON_ERROR(get_coerce_double(env, bound[0], &phi1));
    FAIL_ON_ERROR(get_coerce_double(env, bound[1], &lambda1));
    FAIL_ON_ERROR(get_coerce_double(env, bound[2], &phi2));
    FAIL_ON_ERROR(get_coerce_double(env, bound[3], &lambda2));
    FAIL_ON_ERROR(get_coerce_double(env, argv[2], &phi));
    FAIL_ON_ERROR(get_coerce_double(env, argv[3], &lambda));
    FAIL_ON_ERROR(get_coerce_double(env, argv[4], &d));

    if (enif_is_identical(argv[0], NODE)) {

        if (phi >= phi1 && phi <= phi2 && lambda >= lambda1 && lambda <= lambda2) {
            return E_TRUE;
        }

        small_circle_extents(phi, lambda, d, &i_phi1, &i_lambda1, &i_phi2, &i_lambda2);

        if (INTERSECTS(phi1, phi2, i_phi1, i_phi2)) {
            if (INTERSECTS(lambda1, lambda2, i_lambda1, i_lambda2)) {
                return E_TRUE;
            }
            if (i_lambda1 < -M_PI) {
                i_lambda1 += M_PI * 2.0;
                i_lambda2 += M_PI * 2.0;
                return INTERSECTS(lambda1, lambda2, i_lambda1, i_lambda2) ? E_TRUE : E_FALSE;
            }
            if (i_lambda2 > M_PI) {
                i_lambda1 -= M_PI * 2.0;
                i_lambda2 -= M_PI * 2.0;
                return INTERSECTS(lambda1, lambda2, i_lambda1, i_lambda2) ? E_TRUE : E_FALSE;
            }
        }

        return E_FALSE;

    }

    return (distance((phi1 + phi2) / 2.0, (lambda1 + lambda2) / 2.0, phi, lambda) > d) ?
        E_FALSE : E_TRUE;

}

static ERL_NIF_TERM _in_between(ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[]) {

    int bound_arity;
    const ERL_NIF_TERM * bound;

    double phi, lambda, far_d, close_d, midpoint_d;
    double phi1, lambda1, phi2, lambda2;
    double i_phi1, i_lambda1, i_phi2, i_lambda2;

    FAIL_ON_ERROR(argc == 6);
    FAIL_ON_ERROR(enif_get_tuple(env, argv[1], &bound_arity, &bound));
    FAIL_ON_ERROR(bound_arity == 4);
    FAIL_ON_ERROR(get_coerce_double(env, bound[0], &phi1));
    FAIL_ON_ERROR(get_coerce_double(env, bound[1], &lambda1));
    FAIL_ON_ERROR(get_coerce_double(env, bound[2], &phi2));
    FAIL_ON_ERROR(get_coerce_double(env, bound[3], &lambda2));
    FAIL_ON_ERROR(get_coerce_double(env, argv[2], &phi));
    FAIL_ON_ERROR(get_coerce_double(env, argv[3], &lambda));
    FAIL_ON_ERROR(get_coerce_double(env, argv[4], &far_d));
    FAIL_ON_ERROR(get_coerce_double(env, argv[5], &close_d));

    if (enif_is_identical(argv[0], NODE)) {

        if (phi >= phi1 && phi <= phi2 && lambda >= lambda1 && lambda <= lambda2) {
            goto not_inside;
        }

        small_circle_extents(phi, lambda, close_d, &i_phi1, &i_lambda1, &i_phi2, &i_lambda2);

        if (INTERSECTS(phi1, phi2, i_phi1, i_phi2)) {
            if (INTERSECTS(lambda1, lambda2, i_lambda1, i_lambda2)) {
                goto not_inside;
            }
            if (i_lambda1 < -M_PI) {
                i_lambda1 += M_PI * 2.0;
                i_lambda2 += M_PI * 2.0;
                if (INTERSECTS(lambda1, lambda2, i_lambda1, i_lambda2)) {
                    goto not_inside;
                }
            }
            if (i_lambda2 > M_PI) {
                i_lambda1 -= M_PI * 2.0;
                i_lambda2 -= M_PI * 2.0;
                if (INTERSECTS(lambda1, lambda2, i_lambda1, i_lambda2)) {
                    goto not_inside;
                }
            }
        }

        return E_FALSE;

    }

    midpoint_d = distance((phi1 + phi2) / 2.0, (lambda1 + lambda2) / 2.0, phi, lambda);
    return (midpoint_d >= far_d && midpoint_d <= close_d) ? E_TRUE : E_FALSE;

not_inside:

    return (
        distance(phi, lambda, phi1, lambda1) >= far_d ||
        distance(phi, lambda, phi2, lambda2) >= far_d ||
        distance(phi, lambda, phi1, lambda2) >= far_d ||
        distance(phi, lambda, phi2, lambda1) >= far_d
    ) ? E_TRUE : E_FALSE;

}

//
// INIT

static ErlNifFunc funcs[] = {

    // distance(Phi1, Lambda1, Phi2, Lambda2)
    {"distance", 4, _distance}

    // closer_than(Type, Bound, RPhi, RLambda, Dist)
    , {"closer_than", 5, _closer_than}

    // in_between(Type, Bound, RPhi, RLambda, FDist, CDist)
    , {"in_between", 6, _in_between}

};

ERL_NIF_INIT(erstar_geo, funcs, &on_load, NULL, &on_upgrade, &on_unload);

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

static ERL_NIF_TERM E_NODE;
static ERL_NIF_TERM E_LEAF;
static ERL_NIF_TERM E_FALSE;
static ERL_NIF_TERM E_TRUE;
static ERL_NIF_TERM E_TRUE_FOR_ALL;

typedef struct {
    double phi1, lambda1, phi2, lambda2;
} erstar_bound;

inline ERL_NIF_TERM atom(ErlNifEnv * env, const char * name);

static int on_load(ErlNifEnv * env, void ** priv, ERL_NIF_TERM info) {
    E_NODE = atom(env, "node");
    E_LEAF = atom(env, "leaf");
    E_FALSE = atom(env, "false");
    E_TRUE = atom(env, "true");
    E_TRUE_FOR_ALL = atom(env, "true_for_all");
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

inline int get_bound(ErlNifEnv * env, ERL_NIF_TERM arg, erstar_bound * bp) {

    int arity;
    const ERL_NIF_TERM * tuple;

    if (enif_get_tuple(env, arg, &arity, &tuple) && arity == 4) {
        return
            get_coerce_double(env, tuple[0], &(bp->phi1)) &&
            get_coerce_double(env, tuple[1], &(bp->lambda1)) &&
            get_coerce_double(env, tuple[2], &(bp->phi2)) &&
            get_coerce_double(env, tuple[3], &(bp->lambda2));
    }

    return 0;

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

inline int has_intersection(const erstar_bound * a, const erstar_bound * b) {

    double max_phi1, min_phi2, max_lambda1, min_lambda2;

    max_phi1 = MAX(a->phi1, b->phi1);
    min_phi2 = MIN(b->phi2, a->phi2);

    if (max_phi1 < min_phi2) {
        max_lambda1 = MAX(a->lambda1, b->lambda1);
        min_lambda2 = MIN(b->lambda2, a->lambda2);
        if (max_lambda1 < min_lambda2) {
            return (
                max_phi1 == a->phi1 && min_phi2 == a->phi2 &&
                max_lambda1 == a->lambda1 && min_lambda2 == a->lambda2
            ) ? 2 : 1;
        }
        if (b->lambda1 < -M_PI) {
            max_lambda1 = MAX(b->lambda1 + M_PI * 2.0, a->lambda1);
            min_lambda2 = MIN(a->lambda2, b->lambda2 + M_PI * 2.0);
            return (max_lambda1 < min_lambda2) ? 1 : 0;
        }
        if (b->lambda2 > M_PI) {
            max_lambda1 = MAX(b->lambda1 - M_PI * 2.0, a->lambda1);
            min_lambda2 = MIN(a->lambda2, b->lambda2 - M_PI * 2.0);
            return (max_lambda1 < min_lambda2) ? 1 : 0;
        }
    }

    return 0;

}

inline int inside_small_circle(const erstar_bound * b, double phi, double lambda, double d) {
    return
        distance(b->phi1, b->lambda1, phi, lambda) <= d &&
        distance(b->phi2, b->lambda2, phi, lambda) <= d &&
        distance(b->phi1, b->lambda2, phi, lambda) <= d &&
        distance(b->phi2, b->lambda1, phi, lambda) <= d;
}

static ERL_NIF_TERM _distance(ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[]) {

    double phi1, lambda1, phi2, lambda2;

    FAIL_ON_ERROR(get_coerce_double(env, argv[0], &phi1));
    FAIL_ON_ERROR(get_coerce_double(env, argv[1], &lambda1));
    FAIL_ON_ERROR(get_coerce_double(env, argv[2], &phi2));
    FAIL_ON_ERROR(get_coerce_double(env, argv[3], &lambda2));

    return enif_make_double(env, distance(phi1, lambda1, phi2, lambda2));

}

static ERL_NIF_TERM _small_circle_extents(ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[]) {

    double phi, lambda, d;
    double phi1, lambda1, phi2, lambda2;

    FAIL_ON_ERROR(get_coerce_double(env, argv[0], &phi));
    FAIL_ON_ERROR(get_coerce_double(env, argv[1], &lambda));
    FAIL_ON_ERROR(get_coerce_double(env, argv[2], &d));

    small_circle_extents(phi, lambda, d, &phi1, &lambda1, &phi2, &lambda2);

    return enif_make_tuple(
        env, 4,
        enif_make_double(env, phi1), enif_make_double(env, lambda1),
        enif_make_double(env, phi2), enif_make_double(env, lambda2)
    );

}

static ERL_NIF_TERM _closer_than(ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[]) {

    double phi, lambda, d;
    erstar_bound b, ex;

    FAIL_ON_ERROR(get_bound(env, argv[1], &b));
    FAIL_ON_ERROR(get_coerce_double(env, argv[3], &phi));
    FAIL_ON_ERROR(get_coerce_double(env, argv[4], &lambda));
    FAIL_ON_ERROR(get_coerce_double(env, argv[5], &d));

    if (enif_is_identical(argv[0], E_NODE)) {
        FAIL_ON_ERROR(get_bound(env, argv[2], &ex));
        switch (has_intersection(&b, &ex)) {
            case 2:
                if (inside_small_circle(&b, phi, lambda, d)) {
                    return E_TRUE_FOR_ALL;
                }
            case 1:
                return E_TRUE;
            default:
                return E_FALSE;
        }
    }

    return (distance((b.phi1 + b.phi2) / 2.0, (b.lambda1 + b.lambda2) / 2.0, phi, lambda) > d) ?
        E_FALSE : E_TRUE;

}

static ERL_NIF_TERM _in_between(ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[]) {

    double phi, lambda, far_d, close_d, midpoint_d;
    erstar_bound b, ex;

    FAIL_ON_ERROR(get_bound(env, argv[1], &b));
    FAIL_ON_ERROR(get_coerce_double(env, argv[3], &phi));
    FAIL_ON_ERROR(get_coerce_double(env, argv[4], &lambda));
    FAIL_ON_ERROR(get_coerce_double(env, argv[5], &far_d));
    FAIL_ON_ERROR(get_coerce_double(env, argv[6], &close_d));

    if (enif_is_identical(argv[0], E_NODE)) {

        FAIL_ON_ERROR(get_bound(env, argv[2], &ex));

        int i = 0, m = 0;
        double d[4];

        switch (has_intersection(&b, &ex)) {
            case 2:
                d[0] = distance(b.phi1, b.lambda1, phi, lambda);
                if (d[0] > close_d) {
                    return E_TRUE;
                }
                d[1] = distance(b.phi2, b.lambda2, phi, lambda);
                if (d[1] > close_d) {
                    return E_TRUE;
                }
                d[2] = distance(b.phi1, b.lambda2, phi, lambda);
                if (d[2] > close_d) {
                    return E_TRUE;
                }
                d[3] = distance(b.phi2, b.lambda1, phi, lambda);
                if (d[3] > close_d) {
                    return E_TRUE;
                }
                for (; i < 4; ++i) {
                    m += d[i] < far_d ? 0 : 1;
                }
                if (0 == m) {
                    return E_FALSE;
                }
                if (
                    4 == m &&
                    (b.phi1 - phi) * (b.phi2 - phi) > 0 &&
                    (b.lambda1 - lambda) * (b.lambda2 - lambda) > 0
                ) {
                    return E_TRUE_FOR_ALL;
                }
            case 1:
                return E_TRUE;
            default:
                return E_FALSE;
        }

    }

    midpoint_d = distance((b.phi1 + b.phi2) / 2.0, (b.lambda1 + b.lambda2) / 2.0, phi, lambda);
    return (midpoint_d >= far_d && midpoint_d <= close_d) ? E_TRUE : E_FALSE;

}

//
// INIT

static ErlNifFunc funcs[] = {

    // distance(Phi1, Lambda1, Phi2, Lambda2)
    {"distance", 4, _distance}

    // small_circle_extents(RPhi, RLambda, Dist)
    , {"small_circle_extents", 3, _small_circle_extents}

    // closer_than(Type, Bound, Extents, RPhi, RLambda, Dist)
    , {"closer_than", 6, _closer_than}

    // in_between(Type, Bound, Extents, RPhi, RLambda, FDist, CDist)
    , {"in_between", 7, _in_between}

};

ERL_NIF_INIT(erstar_geo, funcs, &on_load, NULL, &on_upgrade, &on_unload);

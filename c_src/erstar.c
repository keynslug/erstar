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

//
// Module-wide activities

static ERL_NIF_TERM AREA;
static ERL_NIF_TERM OVERLAP;

inline ERL_NIF_TERM atom(ErlNifEnv * env, const char * name);

static int on_load(ErlNifEnv * env, void ** priv, ERL_NIF_TERM info) {
    AREA = atom(env, "area");
    OVERLAP = atom(env, "overlap");
    return 0;
}

static int on_upgrade(ErlNifEnv * env, void ** priv, void ** old_priv, ERL_NIF_TERM info) {
    return on_load(env, priv, info);
}

static void on_unload(ErlNifEnv * env, void * priv) {
}

//

typedef struct {
    double x1, y1, x2, y2;
} erstar_bound;

typedef struct {
    ERL_NIF_TERM node;
    erstar_bound b;
} erstar_node;

inline int coerce_double(ErlNifEnv * env, ERL_NIF_TERM arg, double * dp) {
    ErlNifSInt64 i64;
    return enif_get_double(env, arg, dp) || (
        enif_get_int64(env, arg, &i64) && (*dp = (double)(i64), 1)
    );
}

inline int get_bound(ErlNifEnv * env, ERL_NIF_TERM arg, erstar_bound * pbound) {
    int arity;
    const ERL_NIF_TERM * terms;
    return (
        enif_get_tuple(env, arg, &arity, &terms) && arity == 4 &&
        coerce_double(env, terms[0], &pbound->x1) &&
        coerce_double(env, terms[1], &pbound->y1) &&
        coerce_double(env, terms[2], &pbound->x2) &&
        coerce_double(env, terms[3], &pbound->y2)
    ) ? 1 : 0;
}

inline int get_node_bound(ErlNifEnv * env, ERL_NIF_TERM arg, erstar_bound * pbound) {
    int arity;
    const ERL_NIF_TERM * terms;
    return (
        enif_get_tuple(env, arg, &arity, &terms) && arity == 3 &&
        enif_get_tuple(env, terms[1], &arity, &terms) && arity == 4 &&
        coerce_double(env, terms[0], &pbound->x1) &&
        coerce_double(env, terms[1], &pbound->y1) &&
        coerce_double(env, terms[2], &pbound->x2) &&
        coerce_double(env, terms[3], &pbound->y2)
    ) ? 1 : 0;
}

inline ERL_NIF_TERM atom(ErlNifEnv * env, const char * name) {
    ERL_NIF_TERM ret;
    if (enif_make_existing_atom(env, name, &ret, ERL_NIF_LATIN1)) {
        return ret;
    }
    return enif_make_atom(env, name);
}

inline double min(double a, double b) {
    return (a < b ? a : b);
}

inline double max(double a, double b) {
    return (a > b ? a : b);
}

inline double overlap(erstar_bound * a, erstar_bound * b) {
    double w, h;
    w = min(a->x2, b->x2) - max(a->x1, b->x1);
    if (w > 0.0) {
        h = min(a->y2, b->y2) - max(a->y1, b->y1);
        return max(0.0, w * h);
    }
    return 0.0;
}

//
// Native implementations

#define FAIL_ON_ERROR(e) \
    if (!(e)) { return enif_make_badarg(env); }

static inline double compute_area_difference(
    erstar_bound * bound, erstar_bound * node_bound, erstar_node * nodes, unsigned n
) {
    return
        (max(bound->x2, node_bound->x2) - min(bound->x1, node_bound->x1)) *
        (max(bound->y2, node_bound->y2) - min(bound->y1, node_bound->y1)) -
        (node_bound->x2 - node_bound->x1) *
        (node_bound->y2 - node_bound->y1);
}

static inline double compute_overlap_difference(
    erstar_bound * bound, erstar_bound * node_bound, erstar_node * nodes, unsigned n
) {

    int i;
    double acc = 0.0;
    erstar_bound uni_bound = {
        min(bound->x1, node_bound->x1),
        min(bound->y1, node_bound->y1),
        max(bound->x2, node_bound->x2),
        max(bound->y2, node_bound->y2)
    };

    for (i = 0; i < n; ++i) {
        acc += overlap(&uni_bound, &(nodes[i].b)) - overlap(node_bound, &(nodes[i].b));
    }

    return acc;

}

static ERL_NIF_TERM _needs_least(ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[]) {

    erstar_bound arg_bound;
    double least, criteria;

    unsigned nodes_count, m, i;
    erstar_node * nodes;

    ERL_NIF_TERM head, tail, result;

    double (* compute)(erstar_bound *, erstar_bound *, erstar_node *, unsigned) =
        &compute_area_difference;

    FAIL_ON_ERROR(argc == 3);
    FAIL_ON_ERROR(get_bound(env, argv[1], &arg_bound));

    if (enif_is_identical(argv[0], OVERLAP)) {
        compute = &compute_overlap_difference;
    }

    tail = argv[2];
    FAIL_ON_ERROR(enif_get_list_length(env, tail, &nodes_count));
    FAIL_ON_ERROR(nodes_count);

    nodes = enif_alloc(nodes_count * sizeof(erstar_node));

    i = 0;
    while (enif_get_list_cell(env, tail, &head, &tail)) {
        nodes[i].node = head;
        get_node_bound(env, head, &(nodes[i].b));
        i += 1;
    }

    m = 0;
    least = compute(&arg_bound, &(nodes[m].b), nodes, nodes_count);
    for (i = 1; i < nodes_count; ++i) {
        if (least > (criteria = compute(&arg_bound, &(nodes[i].b), nodes, nodes_count))) {
            least = criteria;
            m = i;
        }
    }

    result = nodes[m].node;
    enif_free(nodes);
    return result;

}

//
// INIT

static ErlNifFunc funcs[] = {

    // needs_least(Criteria, Bound, Nodes)
    {"needs_least", 3, _needs_least}

};

ERL_NIF_INIT(erstar, funcs, &on_load, NULL, &on_upgrade, &on_unload);

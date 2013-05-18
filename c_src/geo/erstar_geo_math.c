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

#include "math.h"

double distance(double phi1, double lambda1, double phi2, double lambda2) {

    double sin_phi1, cos_phi1;
    double sin_phi2, cos_phi2;
    double d_lambda, sin_d_lambda, cos_d_lambda;

    d_lambda = lambda2 - lambda1;
    sincos(phi1, &sin_phi1, &cos_phi1);
    sincos(phi2, &sin_phi2, &cos_phi2);
    sincos(d_lambda, &sin_d_lambda, &cos_d_lambda);

    return atan2(
        sqrt(
            pow(cos_phi2 * sin_d_lambda, 2.0) +
            pow(cos_phi1 * sin_phi2 - sin_phi1 * cos_phi2 * cos_d_lambda, 2.0)
        ),
        sin_phi1 * sin_phi2 + cos_phi1 * cos_phi2 * cos_d_lambda
    );

}

int small_circle_lambda_by_phi(double phi0, double lambda0, double d, double phi, double * p) {

    double sin_phi0, cos_phi0;
    double sin_phi, cos_phi;
    double cos_mult, cos_div;

    sincos(phi0, &sin_phi0, &cos_phi0);
    sincos(phi, &sin_phi, &cos_phi);

    cos_mult = cos_phi0 * cos_phi;
    if (cos_mult == 0.0) {
        return 0;
    }

    cos_div = (cos(d) - sin_phi0 * sin_phi) / cos_mult;
    if (cos_div == -1.0 || cos_div == 1.0) {
        *p = 0.0;
        return 1;
    }
    if (cos_div < -1.0 || cos_div > 1.0) {
        return 0;
    }

    *p = acos(cos_div);
    return 1;

}

int small_circle_phi_by_lambda(
    double phi0, double lambda0, double d, double lambda, double * phi1, double * phi2
) {

    double sin_phi0, cos_phi0;
    double d_lambda, sin_d_lambda, cos_d_lambda;
    double cos_mult, denom, phi, gamma, cos_div;

    d_lambda = lambda - lambda0;
    sincos(phi0, &sin_phi0, &cos_phi0);
    sincos(d_lambda, &sin_d_lambda, &cos_d_lambda);

    cos_mult = cos_phi0 * cos_d_lambda;
    denom = sqrt(sin_phi0 * sin_phi0 + cos_mult * cos_mult);
    if (denom == 0.0) {
        return 0;
    }

    cos_div = cos(d) / denom;
    if (cos_div < -1.0 || cos_div > 1.0) {
        return 0;
    }

    phi = asin(cos_div);
    gamma = acos(sin_phi0 / denom);
    *phi1 = phi - gamma;
    if (phi == M_PI_2) {
        return 0;
    }
    *phi2 = M_PI - phi - gamma;
    return 1;

}

void small_circle_extents(
    double phi0, double lambda0, double d,
    double * phi1, double * lambda1, double * phi2, double * lambda2
) {

    double sin_div, lambda;

    *phi1 = phi0 - d;
    *phi2 = phi0 + d;

    if (d >= M_PI_2) {
        *lambda1 = -M_PI;
        *lambda2 = +M_PI;
        return;
    }

    sin_div = sin(phi0) / cos(d);
    if (sin_div >= 1.0 || sin_div <= -1.0) {
        *lambda1 = -M_PI;
        *lambda2 = +M_PI;
    }
    else {
        small_circle_lambda_by_phi(phi0, lambda0, d, asin(sin_div), &lambda);
        *lambda1 = lambda0 - lambda;
        *lambda2 = lambda0 + lambda;
    }

}

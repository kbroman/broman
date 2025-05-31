/**********************************************************************
 *
 * runningratio2.c
 *
 * copyright (c) 2025, Karl W Broman
 *
 *     This program is free software; you can redistribute it and/or
 *     modify it under the terms of the GNU General Public License,
 *     version 3, as published by the Free Software Foundation.
 *
 *     This program is distributed in the hope that it will be useful,
 *     but without any warranty; without even the implied warranty of
 *     merchantability or fitness for a particular purpose.  See the GNU
 *     General Public License, version 3, for more details.
 *
 *     A copy of the GNU General Public License, version 3, is available
 *     at https://www.r-project.org/Licenses/GPL-3
 *
 * C functions for the R/broman package
 *
 * This is for calculating a running ratio with an adaptive window
 *
 * Contains: runningratio2, R_runningratio2
 *
 **********************************************************************/

#include <math.h>
#include <stdlib.h>
#include <stdio.h>
#include <R.h>
#include <Rmath.h>
#include <R_ext/PrtUtil.h>
#include <R_ext/Applic.h>
#include <R_ext/Utils.h>
#include <R_ext/Arith.h>
#include "runningratio2.h"

/**********************************************************************
 * runningratio2
 *
 * Take sum(numerator)/sum(denominator) in sliding window
 * window is not fixed-width, but to give a target denominator
 *
 **********************************************************************/
void runningratio2(int n, double *pos, double *numerator, double *denominator,
                   int n_result, double *resultpos, double *result, double window_denom)
{
    int lo, ns;
    int i, j;
    double top, bottom;

    window_denom /= 2.0;

    lo=0;
    for(i=0; i<n_result; i++) {

        R_CheckUserInterrupt(); /* check for ^C */

        top = bottom = 0.0;  ns=0;
        for(j=lo; j<n; j++) {
            if(pos[j] < resultpos[i]-window_denom) lo = j+1;
            else if(pos[j] > resultpos[i]+window_denom) break;
            else {
                top += numerator[j];
                bottom += denominator[j];
                ns++;
            }
        }

        if(ns==0) result[i] = NA_REAL;
        else result[i] = (top / bottom);

    }

}

/* wrapper for R */
void R_runningratio2(int *n, double *pos, double *numerator, double *denominator,
                    int *n_result, double *resultpos, double *result, double *window_denom)
{
    runningratio2(*n, pos, numerator, denominator, *n_result, resultpos, result, *window_denom);
}


/* end of runningratio2.c */

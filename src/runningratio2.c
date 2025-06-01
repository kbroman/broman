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
    int i, j, closest;
    int left, right;
    double top, bottom, min_d, d, dleft, dright;

    /* get overall denominator; if <= window_denom, just return overall average for all positions */
    top = bottom = 0.0;
    for(i=0; i<n; i++) {
        top += numerator[i];
        bottom += denominator[i];
    }
    if(bottom <= window_denom || n==1) {
        for(i=0; i<n_result; i++) result[i] = top/bottom;
        return;
    }


    /* Read R's random seed */
    GetRNGstate();

    for(i=0; i<n_result; i++) {

        R_CheckUserInterrupt(); /* check for ^C */

        /* find closest pos to resultpos */
        closest = 0;
        min_d = fabs(pos[0] - resultpos[i]);
        for(j=1; j<n; j++) {
            d = fabs(pos[j] - resultpos[i]);
            if((d < min_d) || (d == min_d && unif_rand() < 0.5)) { /* if tie; choose at random */
                closest = j;
                min_d = d;
            }
        }

        top = numerator[closest];
        bottom = denominator[closest];
        left = right = closest;
        while(bottom < window_denom) {
            if(left > 0 && right < n_result-1) { /* look at which is closer, left or right? */
                dleft = fabs(resultpos[i] - pos[left-1]);
                dright = fabs(resultpos[i] - pos[right+1]);
                if((dleft < dright) || (dleft==dright && unif_rand() < 0.5)) { /* if tie, choose at random */
                    left--;
                    top += numerator[left];
                    bottom += denominator[left];
                }
                else {
                    right++;
                    top += numerator[right];
                    bottom += denominator[right];
                }
            }
            else if(left > 0) { /* have already hit right boundary so just look to left */
                left--;
                top += numerator[left];
                bottom += denominator[left];
            }
            else if(right < n_result - 1) { /* have already hit left boundary so just look to right */
                right++;
                top += numerator[right];
                bottom += denominator[right];
            }
            else { /* hitting the full range */
                break;
            }
        }

        result[i] = top/bottom;

    }

}

/* wrapper for R */
void R_runningratio2(int *n, double *pos, double *numerator, double *denominator,
                    int *n_result, double *resultpos, double *result, double *window_denom)
{
    runningratio2(*n, pos, numerator, denominator, *n_result, resultpos, result, *window_denom);
}


/* end of runningratio2.c */

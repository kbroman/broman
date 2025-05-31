/**********************************************************************
 *
 * runningratio2.h
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

/**********************************************************************
 * runningratio2
 *
 * Take sum(numerator)/sum(denominator) in sliding window
 * window is not fixed-width, but to give a target denominator
 *
 **********************************************************************/
void runningratio2(int n, double *pos, double *numerator, double *denominator,
                   int n_result, double *resultpos, double *result, double window_denom);

/* wrapper for R */
void R_runningratio2(int *n, double *pos, double *numerator, double *denominator,
                     int *n_result, double *resultpos, double *result, double *window_denom);

/* end of runningratio2.h */

/**********************************************************************
 * 
 * runningmean.h
 *
 * copyright (c) 2006-9, Karl W Broman
 *
 * last modified Nov, 2009
 * first written Dec, 2006
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
 *     at http://www.r-project.org/Licenses/GPL-3
 *
 * C functions for the R/broman package
 *
 * This is for calculating a running mean/sum/median.
 * Also for calculating a running ratio.
 *
 * Contains: runningmean, R_runningmean, runningratio, R_runningratio
 *  
 **********************************************************************/

/**********************************************************************
 * runningmean
 *
 * Get running mean or sum within a specified bp-width window
 *
 * method = 1 -> sum
 *        = 2 -> mean
 *        = 3 -> median
 *
 **********************************************************************/
void runningmean(int n, double *pos, double *value, int n_result,
		 double *resultpos, double *result, 
		 double window, int method);

/* wrapper for R */
void R_runningmean(int *n, double *pos, double *value, int *n_result, 
		   double *resultpos, double *result, double *window,
		   int *method);

/**********************************************************************
 * runningratio
 *
 * Take sum(numerator)/sum(denominator) in sliding window
 *
 **********************************************************************/
void runningratio(int n, double *pos, double *numerator, double *denominator,
		  int n_result, double *resultpos, double *result, double window);

/* wrapper for R */
void R_runningratio(int *n, double *pos, double *numerator, double *denominator,
		    int *n_result, double *resultpos, double *result, double *window);

/* end of runningmean.h */

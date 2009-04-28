/**********************************************************************
 * 
 * runningmean.h
 *
 * copyright (c) 2006-7, Karl W Broman
 *
 * last modified Sep, 2007
 * first written Dec, 2006
 *
 * Licensed under the GNU General Public License version 2 (June, 1991)
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
void runningmean(int n, double *pos, double *value, double *result, 
		 double window, int method);

/* wrapper for R */
void R_runningmean(int *n, double *pos, double *value, double *result, double *window,
		   int *method);

/**********************************************************************
 * runningratio
 *
 * Take sum(numerator)/sum(denominator) in sliding window
 *
 **********************************************************************/
void runningratio(int n, double *pos, double *numerator, double *denominator,
		  double *result, double window);

/* wrapper for R */
void R_runningratio(int *n, double *pos, double *numerator, double *denominator,
		    double *result, double *window);

/* end of runningmean.h */

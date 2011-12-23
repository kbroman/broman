/**********************************************************************
 * 
 * runningmean.c
 *
 * copyright (c) 2006-2011, Karl W Broman
 *
 * last modified Dec, 2011
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

#include <math.h>
#include <stdlib.h>
#include <stdio.h>
#include <R.h>
#include <Rmath.h>
#include <R_ext/PrtUtil.h>
#include <R_ext/Applic.h>
#include <R_ext/Utils.h>
#include <R_ext/Arith.h>
#include "runningmean.h"

/**********************************************************************
 * runningmean
 *
 * Get running mean or sum within a specified bp-width window
 *
 * method = 1 -> sum
 *        = 2 -> mean
 *        = 3 -> median
 *        = 4 -> sd
 *
 * We assume that pos and resultpos are both sorted (lo to high)
 *
 **********************************************************************/
void runningmean(int n, double *pos, double *value, 
		 int n_result, 
		 double *resultpos, double *result, 
		 double window, int method)
{
  int lo, ns;
  int i, j;
  double *work3, work4;
  
  
  if(method==3)
    work3 = (double *)R_alloc(n, sizeof(double));

  window /= 2.0;

  lo=0; 
  for(i=0; i<n_result; i++) {

    R_CheckUserInterrupt(); /* check for ^C */

    work4 = result[i] = 0.0; ns=0;
    for(j=lo; j<n; j++) {
      if(pos[j] < resultpos[i]-window) lo = j+1;
      else if(pos[j] > resultpos[i]+window) break;
      else {

	if(method==1 || method==2 || method==4)
	  result[i] += value[j];
	if(method==3) 
	  work3[ns] = value[j];
	if(method==4)
	  work4 += (value[j]*value[j]);

	ns++;
      }
    }
    if(ns==0 || (method==4 && ns==1)) result[i] = NA_REAL;
    else {
      if(method==2) result[i] /= (double)ns;
      if(method==3) {
	R_rsort(work3, ns);
	if(ns % 2) 
	  result[i] = work3[(ns-1)/2];
	else /* even */
	  result[i] = (work3[ns/2-1]+work3[ns/2])/2.0;
      }
      if(method==4) 
	result[i] = sqrt((work4 - result[i]*result[i]/(double)ns)/((double)(ns-1)));
    }
  }

}

/* wrapper for R */
void R_runningmean(int *n, double *pos, double *value, 
		   int *n_result, double *resultpos, double *result, 
		   double *window, int *method)
{
  runningmean(*n, pos, value, *n_result, resultpos, result, *window, *method);
}


/**********************************************************************
 * runningratio
 *
 * Take sum(numerator)/sum(denominator) in sliding window
 *
 * We assume that pos and resultpos are sorted (lo to high)
 **********************************************************************/
void runningratio(int n, double *pos, double *numerator, double *denominator,
		  int n_result, double *resultpos, double *result, double window)
{
  int lo, ns;
  int i, j;
  double top, bottom;
  
  window /= 2.0;

  lo=0; 
  for(i=0; i<n_result; i++) {

    R_CheckUserInterrupt(); /* check for ^C */

    top = bottom = 0.0;  ns=0;
    for(j=lo; j<n; j++) {
      if(pos[j] < resultpos[i]-window) lo = j+1;
      else if(pos[j] > resultpos[i]+window) break;
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
void R_runningratio(int *n, double *pos, double *numerator, double *denominator,
		    int *n_result, double *resultpos, double *result, double *window)
{
  runningratio(*n, pos, numerator, denominator, *n_result, resultpos, result, *window);
}


/* end of runningmean.c */

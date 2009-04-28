/**********************************************************************
 * 
 * runningmean.c
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

#include <math.h>
#include <stdlib.h>
#include <stdio.h>
#include <R.h>
#include <Rmath.h>
#include <R_ext/PrtUtil.h>
#include <R_ext/Applic.h>
#include <R_ext/Utils.h>
#include "runningmean.h"

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
		 double window, int method)
{
  int lo, ns;
  int i, j;
  double *work;
  
  work = (double *)R_alloc(n, sizeof(double));

  window /= 2.0;

  lo=0; 
  for(i=0; i<n; i++) {

    R_CheckUserInterrupt(); /* check for ^C */

    result[i] = 0.0; ns=0;
    for(j=lo; j<n; j++) {
      if(pos[j] < pos[i]-window) lo = j+1;
      else if(pos[j] > pos[i]+window) break;
      else {

	if(method==1 || method==2) 
	  result[i] += value[j];
	else 
	  work[ns] = value[j];

	ns++;
      }
    }
    if(method==2) result[i] /= (double)ns;
    if(method==3) {
      R_rsort(work, ns);
      if(ns % 2) 
	result[i] = work[(ns-1)/2];
      else /* even */
	result[i] = (work[ns/2-1]+work[ns/2])/2.0;
    }
  }

}

/* wrapper for R */
void R_runningmean(int *n, double *pos, double *value, double *result, double *window,
		   int *method)
{
  runningmean(*n, pos, value, result, *window, *method);
}


/**********************************************************************
 * runningratio
 *
 * Take sum(numerator)/sum(denominator) in sliding window
 *
 **********************************************************************/
void runningratio(int n, double *pos, double *numerator, double *denominator,
		  double *result, double window)
{
  int lo;
  int i, j;
  double top, bottom;
  
  window /= 2.0;

  lo=0; 
  for(i=0; i<n; i++) {

    R_CheckUserInterrupt(); /* check for ^C */

    top = bottom = 0.0; 
    for(j=lo; j<n; j++) {
      if(pos[j] < pos[i]-window) lo = j+1;
      else if(pos[j] > pos[i]+window) break;
      else {
	top += numerator[j];
	bottom += denominator[j];
      }
    }

    result[i] = (top / bottom);

  }

}

/* wrapper for R */
void R_runningratio(int *n, double *pos, double *numerator, double *denominator,
		    double *result, double *window)
{
  runningratio(*n, pos, numerator, denominator, result, *window);
}


/* end of runningmean.c */

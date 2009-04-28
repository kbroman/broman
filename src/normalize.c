/**********************************************************************
 * 
 * runningmean.c
 *
 * copyright (c) 2005, Karl W Broman
 *
 * last modified 28 Oct 2005
 * first written 18 Sep 2005
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
 * Contains: normalize, R_normalize
 *  
 **********************************************************************/

#include <math.h>
#include <stdlib.h>
#include <stdio.h>
#include <R.h>
#include <Rmath.h>
#include <R_ext/PrtUtil.h>
#include <R_ext/Applic.h>
#include "normalize.h"

/**********************************************************************
 * normalize
 * 
 * force two sets of intensities to have the same marginal distributions
 *
 **********************************************************************/
void normalize(int n, int p, double **X, int **iX, double **tX)
{
  int i, j;
  double temp;

  /* sort the data */
  for(j=0; j<p; j++) 
    rsort_with_index(tX[j], iX[j], n);

  /* get averages and substitute original values for these */
  for(i=0; i<n; i++) {
    temp = 0.0;

    for(j=0; j<p; j++) 
      temp += tX[j][i];
    temp /= (double)p;

    for(j=0; j<p; j++) 
      X[j][iX[j][i]] = temp;
  }
}

/* wrapper for R */
void R_normalize(int *n, int *p, double *x, int *ix, double *tx)
{
  double **X, **tX;
  int **iX;

  reorg_dmatrix(*n, *p, x, &X);
  reorg_imatrix(*n, *p, ix, &iX);
  reorg_dmatrix(*n, *p, tx, &tX);

  normalize(*n, *p, X, iX, tX);
}


/**********************************************************************
 * reorg_dmatrix 
 * 
 * reorganize a 2-d matrix of doubles for double-indexing
 *
 **********************************************************************/
void reorg_dmatrix(int nr, int nc, double *x, double ***X)
{
  int i;

  *X = (double **)R_alloc(nc, sizeof(double *));

  (*X)[0] = x;
  for(i=1; i< nc; i++) 
    (*X)[i] = (*X)[i-1] + nr;
}

/**********************************************************************
 * reorg_imatrix 
 * 
 * reorganize a 2-d matrix of ints for double-indexing
 *
 **********************************************************************/
void reorg_imatrix(int nr, int nc, int *x, int ***X)
{
  int i;

  *X = (int **)R_alloc(nc, sizeof(int *));

  (*X)[0] = x;
  for(i=1; i< nc; i++) 
    (*X)[i] = (*X)[i-1] + nr;
}

/* end of normalize.c */

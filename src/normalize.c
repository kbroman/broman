/**********************************************************************
 *
 * normalize.c
 *
 * copyright (c) 2005-2011, Karl W Broman
 *
 * last modified 19 Apr 2011
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
 *     at https://www.r-project.org/Licenses/GPL-3
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
 * force a matrix of intensities to have columns with the same marginal
 * distribution
 *
 **********************************************************************/
void normalize(int n, int p, double **X, double maxval,
               int **iX, double **tX)
{
    int i, j;
    double *ave, fracpart, intpart;
    int *nobs, max_nobs;

    /* count number observed */
    nobs = (int *)R_alloc(p, sizeof(int));
    max_nobs = 0;
    for(j=0; j<p; j++) {
        nobs[j] = 0;
        for(i=0; i<n; i++)
            if(X[j][i] < maxval) nobs[j]++;
        if(nobs[j] > max_nobs) max_nobs = nobs[j]; /* maximum number observed */
    }
    ave = (double *)R_alloc(max_nobs, sizeof(double));


    /* sort the data */
    for(j=0; j<p; j++)
        rsort_with_index(tX[j], iX[j], n);

    /* get averages of sorted data */
    for(i=0; i<max_nobs; i++) {
        ave[i] = 0.0;

        for(j=0; j<p; j++) {
            if(nobs[j] == max_nobs)
                ave[i] += tX[j][i];
            else {
                /* need to be a bit fancy to deal with varying numbers of observed values */
                fracpart = modf((double)i * (double)(nobs[j]-1) / (double)(max_nobs-1), &intpart);

                if(intpart > nobs[j]-1) { intpart = nobs[j]-1; fracpart = 0.0; }
                ave[i] += (tX[j][(int)intpart]*(1.0-fracpart));
                if(intpart <= nobs[j]-2)
                    ave[i] += (tX[j][(int)intpart+1]*fracpart);
            }
        }
        ave[i] /= (double)p;
    }

    /* substitute averages in place of original values */
    for(j=0; j<p; j++) {
        if(nobs[j] == max_nobs)
            for(i=0; i<max_nobs; i++) X[j][iX[j][i]] = ave[i];
        else {
            for(i=0; i<nobs[j]; i++) {
                /* need to be a bit fancy to deal with varying numbers of observed values */
                fracpart = modf((double)i / (double)(nobs[j]-1) * (double)(max_nobs-1), &intpart);
                if(intpart > max_nobs-1) { intpart = max_nobs-1; fracpart = 0.0; }
                X[j][iX[j][i]] = ave[(int)intpart]*(1.0-fracpart);
                if((int)intpart <= max_nobs-2)
                    X[j][iX[j][i]] += ave[(int)intpart+1]*fracpart;
            }
        }
    }
}

/* wrapper for R */
void R_normalize(int *n, int *p, double *x, double *maxval, int *ix, double *tx)
{
    double **X, **tX;
    int **iX;

    reorg_dmatrix(*n, *p, x, &X);
    reorg_imatrix(*n, *p, ix, &iX);
    reorg_dmatrix(*n, *p, tx, &tX);

    normalize(*n, *p, X, *maxval, iX, tX);
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

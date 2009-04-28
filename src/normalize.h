/**********************************************************************
 * 
 * runningmean.h
 *
 * copyright (c) 2005, Karl W Broman
 *
 * last modified 28 Oct 2005
 * first written 18 Sep 2005
 *
 * Licensed under the GNU General Public License version 2 (June, 1991)
 *
 * C functions for the R/broman package
 *
 * Contains: normalize, R_normalize
 *  
 **********************************************************************/

/**********************************************************************
 * normalize
 * 
 * force two sets of intensities to have the same marginal distributions
 *
 **********************************************************************/
void normalize(int n, int p, double **X, int **iX, double **tX);

/* wrapper for R */
void R_normalize(int *n, int *p, double *x, int *ix, double *tx);

/**********************************************************************
 * reorg_dmatrix 
 * 
 * reorganize a 2-d matrix of doubles for double-indexing
 *
 **********************************************************************/
void reorg_dmatrix(int nr, int nc, double *x, double ***X);

/**********************************************************************
 * reorg_imatrix 
 * 
 * reorganize a 2-d matrix of ints for double-indexing
 *
 **********************************************************************/
void reorg_imatrix(int nr, int nc, int *x, int ***X);

/* end of normalize.c */

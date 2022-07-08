/* compare_rows.c

   Karl W Broman

   last modified 25 May 2016
   first written 16 July 2015

*/

#include <math.h>
#include <stdlib.h>
#include <stdio.h>
#include <limits.h>
#include <R.h>
#include <Rmath.h>
#include "compare_rows.h"

/* compare rows by proportion of mismatches */
void compare_rows_mismatch(int **Mat, int nrow, int ncol, double **D)
{
    int i, j, k, n;

    for(i=0; i< nrow-1; i++) {
        for(j=i+1; j<nrow; j++) {
            R_CheckUserInterrupt(); /* check for ^C */

            D[i][j] = 0.0;
            n = 0;
            for(k=0; k<ncol; k++) {
                /* INT_MIN is the missing value for integers */
                if(Mat[k][i] > INT_MIN && Mat[k][j] > INT_MIN) {
                    n++;
                    if(Mat[k][i] != Mat[k][j]) (D[i][j])++;
                }
            }
            if(n==0) D[i][j] = NA_REAL;
            else D[i][j] /= (double)n;

            D[j][i] = D[i][j];
        }
    }
}


/* compare rows by RMS difference */
void compare_rows_rmsd(double **Mat, int nrow, int ncol, double **D)
{
    int i, j, k, n;
    double a;

    for(i=0; i< nrow-1; i++) {
        for(j=i+1; j<nrow; j++) {
            R_CheckUserInterrupt(); /* check for ^C */

            D[i][j] = 0.0;
            n = 0;
            for(k=0; k<ncol; k++) {
                if(!ISNA(Mat[k][i]) && !ISNA(Mat[k][j])) {
                    n++;
                    a = (Mat[k][i] - Mat[k][j]);
                    D[i][j] += (a*a);
                }
            }
            if(n==0) D[i][j] = NA_REAL;
            else D[i][j] = sqrt(D[i][j] / (double)n);

            D[j][i] = D[i][j];
        }
    }
}

/* R wrappers */
void R_compare_rows_mismatch(int *mat, int *nrow, int *ncol, double *d)
{
    int i=0;
    int **Mat;
    double **D;

    Mat = (int **)R_alloc(*ncol, sizeof(int *));
    Mat[0] = mat;
    for(i=1; i< *ncol; i++)
        Mat[i] = Mat[i-1] + *nrow;

    D = (double **)R_alloc(*nrow, sizeof(double *));
    D[0] = d;
    for(i=1; i< *nrow; i++)
        D[i] = D[i-1] + *nrow;

    compare_rows_mismatch(Mat, *nrow, *ncol, D);
}

void R_compare_rows_rmsd(double *mat, int *nrow, int *ncol, double *d)
{
    int i=0;
    double **Mat;
    double **D;

    Mat = (double **)R_alloc(*ncol, sizeof(double *));
    Mat[0] = mat;
    for(i=1; i< *ncol; i++)
        Mat[i] = Mat[i-1] + *nrow;

    D = (double **)R_alloc(*nrow, sizeof(double *));
    D[0] = d;
    for(i=1; i< *nrow; i++)
        D[i] = D[i-1] + *nrow;

    compare_rows_rmsd(Mat, *nrow, *ncol, D);
}

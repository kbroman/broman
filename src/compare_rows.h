/* compare_rows.h

   Karl W Broman

   last modified 16 July 2015
   first written 16 July 2015

*/

/* compare rows by proportion of mismatches */
void compare_rows_mismatch(int **Mat, int nrow, int ncol, double **D);

/* compare rows by RMS difference */
void compare_rows_rmsd(double **Mat, int nrow, int ncol, double **D);

/* R wrappers */
void R_compare_rows_mismatch(int *mat, int *nrow, int *ncol, double *d);
void R_compare_rows_rmsd(double *mat, int *nrow, int *ncol, double *d);

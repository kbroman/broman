/* for each value, count number of other values
   that are within some tolerance (tol)
*/

void count_close(double *values, int n_values, double tol, int *counts);

void R_count_close(double *values, int *n_values, double *tol, int *counts);

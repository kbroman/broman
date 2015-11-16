/* for each value, count number of other values
   that are within some tolerance (tol)
*/

#include <math.h>
#include <stdlib.h>
#include "count_close.h"

void count_close(double *values, int n_values, double tol, int *counts)
{
    int i, j;

    /* assume counts initialized at 0 */

    for(i=0; i<n_values-1; i++) {
        for(j=i+1; j<n_values; j++) {
            if(fabs(values[i] - values[j]) <= tol) {
                counts[i]++;
                counts[j]++;
            }
        }
    }
}

void R_count_close(double *values, int *n_values, double *tol, int *counts)
{
    count_close(values, *n_values, *tol, counts);
}

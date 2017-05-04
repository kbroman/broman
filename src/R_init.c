// Register Dynamic Symbols

#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>
#include "R_init.h"

void R_init_broman(DllInfo* info) {
    R_registerRoutines(info, NULL, NULL, NULL, NULL);
    R_useDynamicSymbols(info, TRUE);
}

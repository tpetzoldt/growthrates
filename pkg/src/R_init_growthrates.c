#ifndef R_R_H
# include <R.h>
#endif

#ifndef R_EXT_DYNLOAD_H_
# include <R_ext/Rdynload.h>
#endif

#include <Rinternals.h>
#include <stdlib.h> // for NULL


extern void ini_twostep(void (* odeparms)(int *, double *));
extern void d_twostep(int *neq, double *t, double *y, double *ydot, double *yout, int*ip);

extern void ini_genlogistic(void (* odeparms)(int *, double *));
extern void d_genlogistic(int *neq, double *t, double *y, double *ydot, double *yout, int*ip);
  

static const R_CMethodDef CEntries[] = {
    {"ini_twostep",     (DL_FUNC) &ini_twostep,     1},
    {"d_twostep",       (DL_FUNC) &d_twostep,       6}, 
    {"ini_genlogistic", (DL_FUNC) &ini_genlogistic, 1},
    {"d_genlogistic",   (DL_FUNC) &d_genlogistic,   6}, 
    {NULL, NULL, 0}
}; 

static const R_CallMethodDef CallEntries[] = {
    {NULL,        NULL,               0}
};

void R_init_growthrates(DllInfo *dll) {
  // register entry points
  R_registerRoutines(dll, CEntries, CallEntries, NULL, NULL);
  
  // the following two lines protect against accidentially finding entry points
  R_useDynamicSymbols(dll, FALSE);  // disable dynamic searching
  //R_forceSymbols(dll, TRUE);      // entry points as R objects, not as strings
} 

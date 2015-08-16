/* C implementation of generalised logistic differential equation        */
/* The "generalized logistic" follows the definition of  Tsoularis, 2001 */

#include <R.h> /* gives F77_CALL through R_ext/RS.h */

static double parms[5];

/* define 5 parameters as macros: mumax, K, alpha, beta, gamma */
#define mumax parms[0]
#define K     parms[1]
#define alpha parms[2]
#define beta  parms[3]
#define gamma parms[4]

/* initializer */
void ini_genlogistic(void (* odeparms)(int *, double *))
{
    int N=5;
    odeparms(&N, parms);
}

/* derivatives */
void d_genlogistic(int *neq, double *t, double *y, double *ydot, double *yout, int*ip)
{
    if (ip[0] < 0) error("nout should be zero");
    ydot[0] = mumax * pow(*y, alpha) * pow((1-pow(*y/K, beta)), gamma);
}

 

/* C implementation of two-step logistic growth equations                */


#include <R.h> /* gives F77_CALL through R_ext/RS.h */

static double parms[3];

/* define 5 parameters as macros: mumax, K, alpha, beta, gamma */
#define kw    parms[0]
#define mumax parms[1]
#define K     parms[2]


/* initializer */
void ini_twostep(void (* odeparms)(int *, double *))
{
    int N = 3;
    odeparms(&N, parms);
}

/* derivatives */
void d_twostep(int *neq, double *t, double *y, double *ydot, double *yout, int*ip)
{
    if (ip[0] < 1) error("nout should be >= 1");
    ydot[0] = -kw * y[0];
    ydot[1] = kw * y[0] + mumax * (1.0 - (y[0] + y[1])/K) * y[1];

    yout[0] = y[0] + y[1];
    //yout[1] = log(y[0] + y[1]);
}



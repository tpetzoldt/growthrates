/* C implementation of two-step logistic growth equations                */


#include <R.h> /* gives F77_CALL through R_ext/RS.h */

static double parms[3];

/* define 5 parameters as macros: mu, K, alpha, beta, gamma */
#define kw    parms[0]
#define mu    parms[0]
#define K     parms[1]


/* initializer */
void ini_twostep(void (* odeparms)(int *, double *))
{
    int N = 3;
    odeparms(&N, parms);
}

/* derivatives */
void d_twostep(int *neq, double *t, double *y, double *ydot, double *yout, int*ip)
{
    if (ip[0] < 2) error("nout should be >= 2");
    ydot[0] = -kw * y[0];
    ydot[1] = kw * y[0] + mu * (1.0 - (y[0] + y[1])/K) * y[1];

    yout[0] = y[0] + y[1];
    yout[1] = log(y[0] + y[1]);
}

 

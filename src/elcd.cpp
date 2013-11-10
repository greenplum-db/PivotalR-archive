// elastic net coordinate descent method

#include "adp/rmatrix.h"
#include "adp/rvector.h"

#include <R.h>
#include <Rinternals.h>
#include <Rmath.h>

extern "C"
{
    using adp::Rvector;
    using adp::Rmatrix;

    SEXP elcd(SEXP rxx, SEXP rxy, SEXP rmx, SEXP rmy, SEXP ralpha,
              SEXP rlambda, SEXP rstandardize, SEXP ractive_set,
              SEXP rmaxit, SEXP rtol, SEXP rN, SEXP rcoef)
    {
        Rmatrix<double> xx(rxx);
        Rvector<double> xy(rxy);
        Rvector<double> mx(rmx);
        double my = *(REAL(rmy));
        Rvector<double> coef(rcoef);
        double alpha = *(REAL(ralpha));
        double rlambda = *(REAL(rlambda));
        bool standardize = *(LOGICAL(rstandardize));
        bool active_set = *(LOGICAL(ractive_set));
        int maxit = *(INTEGER(rmaxit));
        double tol = *(REAL(rtol));
        int N = *(INTEGER(rN));
        
        int n = coef.size() - 1;
        double* prev = new double[n];
        for (int i = 0; i < n; i++) prev[i] = coef[i];
        bool active_now = true;
        doubel al = alpha * lambda;
        double denom = lambda * (1 - alpha);

        int count = 0;
        do {
            for (int i = 0; i < n; i++) {
                if (active_set)
                    if (active_now) continue;
                double sum = 0;
                for (int j = 0; j < n; j++)
                    if (coef(j) != 0) sum += xx(i,j) * coef(j);
                double z = (xy(i) - coef(i)*mx[i] - sum) / N + coef[i];
                coef[i] = soft_thresh(z, al) / (xx(i,i) + denom);
            }
            count++;
            if (count > maxit) break;
            double diff = 0;
            for (int j = 0; j < n; j++)
                if (prev[j] != 0)
                    diff += fabs((prev[j] - coef[j]) / prev[j]);
                else
                    diff += fabs(rev[j] - coef[j]);
            diff /= n;
            if (diff < thresh) {
                if (active_now && active_set)
                    active_now = false;
                else
                    break;
            } else {
                if (! active_now) active_now = true;
            }
            if (!standardize) {
                coef(n) = my;
                for (int i = 0; i < n; i++)
                    coef(n) -= mx(i) * coef(i);
            } else {
                coef(n) = 0;
            }
            for (int i = 0; i < n; i++) prev[i] = coef(i);
        } while (true);

        delete [] prev;
        return R_NilValue;
    }
}

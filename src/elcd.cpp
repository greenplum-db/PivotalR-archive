// elastic net coordinate descent method

#include "adp/rmatrix.h"
#include "adp/rvector.h"

#include <R.h>
#include <Rinternals.h>
#include <Rmath.h>

#include <vector>
#include <algorithm>    // random_shuffle
#include <ctime>        // std::time
#include <cstdlib>      // std::rand, std::srand

using adp::Rvector;
using adp::Rmatrix;

// ----------------------------------------------------------------------

double soft_thresh (double z, double lambda)
{
    if (z > 0 && z > lambda) return (z - lambda);
    if (z < 0 && -z > lambda) return (z + lambda);
    return 0;
}

// ----------------------------------------------------------------------

std::vector<int> random_index(int n)
{
    std::vector<int> res(n,0);
    for (int i = 0; i < n; i++) res[i] = i;
    std::random_shuffle(res.begin(), res.end());
    return res;
}

// ----------------------------------------------------------------------

SEXP elcd1(SEXP rxx, SEXP rxy, SEXP rmx, SEXP rmy, SEXP rsx, SEXP rsy,
        SEXP ralpha, SEXP rlambda, SEXP rstandardize, SEXP ractive_set,
        SEXP rmaxit, SEXP rtol, SEXP rN, SEXP rcoef, SEXP riter,
        SEXP rloglik)
{
    Rmatrix<double> xx(rxx);
    Rvector<double> xy(rxy);
    Rvector<double> mx(rmx);
    double my = *(REAL(rmy));
    Rvector<double> sx(rsx);
    double sy = *(REAL(rsy));
    Rvector<double> coef(rcoef);
    double alpha = *(REAL(ralpha));
    double lambda = *(REAL(rlambda));
    bool standardize = *(LOGICAL(rstandardize));
    bool active_set = *(LOGICAL(ractive_set));
    int maxit = *(INTEGER(rmaxit));
    double tol = *(REAL(rtol));
    int N = *(INTEGER(rN));
    double* iter = REAL(riter);
    double* loglik = REAL(rloglik);

    int n = coef.size() - 1;
    double* prev = new double[n];
    for (int i = 0; i < n; i++) prev[i] = coef(i);
    bool active_now = true;
    double al = alpha * lambda;
    double denom = lambda * (1 - alpha);

    std::vector<int> idx(n,0);
    for (int i = 0; i < n; i++) idx[i] = i;

    int count = 0;
    do {
        std::random_shuffle(idx.begin(), idx.end());
        for (int id = 0; id < n; id++) {
            int i = idx[id];
            if (active_set && active_now && coef(i) == 0) continue;
            double sum = 0;
            for (int j = 0; j < n; j++)
                if (coef(j) != 0) sum += xx(i,j) * coef(j);
            double z;
            if (standardize)
                z = (xy(i) - sum) / N + coef(i);
            else
                z = (xy(i) - sum) / N - coef(n)*mx(i) +
                    coef(i) * xx(i,i)/N;
            coef(i) = soft_thresh(z, al) / (xx(i,i)/N + denom);
        }
        count++;
        if (count > maxit) break;
        double diff = 0;
        for (int i = 0; i < n; i++)
            if (prev[i] != 0)
                diff += fabs((prev[i] - coef(i)) / prev[i]);
            else
                diff += fabs(prev[i] - coef(i));
        diff /= n;
        if (diff < tol) {
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
        }
        for (int i = 0; i < n; i++) prev[i] = coef(i);
    } while (true);

    // compute the log-likelihood
    *loglik = 0;
    if (standardize) {
        *loglik -= sy * sy * N / 2;
        for (int i = 0; i < n; i++)
            *loglik += coef(i) * xy(i);
    } else {
        *loglik -= sy * sy * N / 2 + (coef(n) - my) * (coef(n) - my) * N / 2;
        for (int i = 0; i < n; i++)
            *loglik -= coef(n) * coef(i) * mx(i) * N
                - coef(i) * xy(i);
    }
    for (int i = 0; i < n; i++)
        for (int j = i; j < n; j++)
            if (i == j)
                *loglik -= 0.5 * coef(i) * coef(j) * xx(i,i);
            else
                *loglik -= coef(i) * coef(j) * xx(i,j);
    *loglik /= N;
    for (int i = 0; i < n; i++)
        *loglik -= denom * coef(i) * coef(i) / 2 +
            alpha * lambda * fabs(coef(i));

    // un-standardize if standardize = TRUE
    if (standardize) {
        coef(n) = my;
        for (int i = 0; i < n; i++) {
            coef(i) = coef(i) / sx(i);
            coef(n) = coef(n) - coef(i) * mx(i);
        }
    }

    *iter = count; // number of iterations

    delete [] prev;
    return R_NilValue;
}

// ----------------------------------------------------------------------

SEXP elcd_binom1(SEXP rxx, SEXP rxy, SEXP rmwx, SEXP rmx,
        SEXP rmwz, SEXP rmw, SEXP ralpha, SEXP rlambda,
        SEXP ractive_set,
        SEXP rmaxit, SEXP rtol, SEXP rN, SEXP rcoef, SEXP riter)
{
    Rmatrix<double> xx(rxx);
    Rvector<double> xy(rxy);
    Rvector<double> mwx(rmwx);
    Rvector<double> mx(rmx);
    // double my = *(REAL(rmy));
    double mwz = *(REAL(rmwz));
    double mw = *(REAL(rmw));
    Rvector<double> coef(rcoef);
    double alpha = *(REAL(ralpha));
    double lambda = *(REAL(rlambda));
    bool active_set = *(LOGICAL(ractive_set));
    int maxit = *(INTEGER(rmaxit));
    double tol = *(REAL(rtol));
    int N = *(INTEGER(rN));
    double* iter = REAL(riter);

    int n = coef.size() - 1;
    double* prev = new double[n];
    for (int i = 0; i < n; i++) prev[i] = coef(i);
    bool active_now = true;
    double al = alpha * lambda;
    double denom = lambda * (1 - alpha);

    std::vector<int> idx(n,0);
    for (int i = 0; i < n; i++) idx[i] = i;

    int count = 0;
    do {
        std::random_shuffle(idx.begin(), idx.end());
        for (int id = 0; id < n; id++) {
            int i = idx[id];
            if (active_set && active_now && coef(i) == 0) continue;
            double sum = 0;
            for (int j = 0; j < n; j++)
                if (coef(j) != 0) sum += xx(i,j) * coef(j);
            double z;
            z = (xy(i) - sum) / N - coef(n)*mwx(i) +
                coef(i) * xx(i,i)/N;
            coef(i) = soft_thresh(z, al) / (xx(i,i)/N + denom);
        }
        count++;
        if (count > maxit) break;
        double diff = 0;
        for (int i = 0; i < n; i++)
            if (prev[i] != 0)
                diff += fabs((prev[i] - coef(i)) / prev[i]);
            else
                diff += fabs(prev[i] - coef(i));
        diff /= n;
        if (diff < tol) {
            if (active_now && active_set)
                active_now = false;
            else
                break;
        } else {
            if (! active_now) active_now = true;
        }
        coef(n) = mwz;
        for (int i = 0; i < n; i++)
            coef(n) -= mwx(i) * coef(i);
        coef(n) /= mw;
        for (int i = 0; i < n; i++) prev[i] = coef(i);
    } while (true);

    *iter = count; // number of iterations

    delete [] prev;
    return R_NilValue;
}

// ------------------------------------------------------------------

extern "C"
{
    SEXP elcd(SEXP rxx, SEXP rxy, SEXP rmx, SEXP rmy, SEXP rsx, SEXP rsy,
        SEXP ralpha, SEXP rlambda, SEXP rstandardize, SEXP ractive_set,
        SEXP rmaxit, SEXP rtol, SEXP rN, SEXP rcoef, SEXP riter,
        SEXP rloglik)
    {
        return elcd1(rxx, rxy, rmx, rmy, rsx, rsy,
            ralpha, rlambda, rstandardize, ractive_set,
            rmaxit, rtol, rN, rcoef, riter,
            rloglik);
    }

    SEXP elcd_binom(SEXP rxx, SEXP rxy, SEXP rmwx, SEXP rmx,
        SEXP rmwz, SEXP rmw, SEXP ralpha, SEXP rlambda,
        SEXP ractive_set,
        SEXP rmaxit, SEXP rtol, SEXP rN, SEXP rcoef, SEXP riter)
    {
        return elcd_binom1(rxx, rxy, rmwx, rmx,
        rmwz, rmw, ralpha, rlambda,
        ractive_set,
        rmaxit, rtol, rN, rcoef, riter);
    }
}

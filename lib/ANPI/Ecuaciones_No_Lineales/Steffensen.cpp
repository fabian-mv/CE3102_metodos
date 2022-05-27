#include <cmath>
#include <cstdio>

#include "Steffensen.h"

extern "C"
{
    double err_steffensen(double (*f)(void*, double), void *arg, double xk)
    {
        return std::fabs(f(arg, xk));
    }

    double iter_steffensen(double (*f)(void*, double), void *arg, double xk)
    {
        auto fxk = f(arg, xk);
        return xk - (fxk * fxk) / (f(arg, xk + fxk) - fxk);
    }
}

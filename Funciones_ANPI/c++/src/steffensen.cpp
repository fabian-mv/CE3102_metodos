#include <cmath>
#include <cstdio>

#include "anpi/steffensen.h"

extern "C"
{
    double err_steffensen(double (*f)(double), double xk)
    {
        return std::fabs(f(xk));
    }

    double iter_steffensen(double (*f)(double), double xk)
    {
        auto fxk = f(xk);
        return xk - (fxk * fxk) / (f(xk + fxk) - fxk);
    }
}

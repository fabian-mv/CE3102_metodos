#include <cmath>
#include <cstdio>

#include "Steffensen.h"
extern "C"
{
    Solucion steffensen(Ld (*f)(Ld), Ld x0, Ld tol, int itermax)
    {
        Ld xk{x0};
        Ld error{0};
        Ld fxk{0};
        int k{0};
        for (int i = 0; i < itermax; i++)
        {
            fxk = f(xk);
            xk = xk - (fxk * fxk) / (f(xk + fxk) - fxk);
            error = std::fabs(f(xk));
            if (error < tol)
            {
                k = i;
                break;
            }
        }
        return Solucion{
            .x = xk,
            .error = error,
            .k = k};
    }
}

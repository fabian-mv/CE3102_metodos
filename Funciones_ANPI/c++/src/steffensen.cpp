#include <cmath>
#include <cstdio>

#include "anpi/ffi.h"

extern "C" double err_steffensen(double (*f)(double), double xk)
{
	return std::fabs(f(xk));
}

extern "C" double iter_steffensen(double (*f)(double), double xk)
{
	auto fxk = f(xk);
	return xk - (fxk * fxk) / (f(xk + fxk) - fxk);
}

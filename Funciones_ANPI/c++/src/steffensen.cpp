#include <cmath>
#include <cstdio>

#include "anpi/ffi.h"

/**
 * Exporta la función err_steffensen para que Haskell la pueda utilizar.
 *
 * Extern "C" hace que el nombre de una función en C++ no sea "mangled" por el
 * compilador. Esto hace que Haskell pueda utilizar la función a través de un
 * header que contiene la declaración de la función.
 * 
 */
extern "C" double err_steffensen(double (*f)(double), double xk)
{
	return std::fabs(f(xk));
}

/**
 * Exporta la función iter_steffensen para que Haskell la pueda utilizar.
 *
 * Extern "C" hace que el nombre de una función en C++ no sea "mangled" por el
 * compilador. Esto hace que Haskell pueda utilizar la función a través de un
 * header que contiene la declaración de la función.
 * 
 */
extern "C" double iter_steffensen(double (*f)(double), double xk)
{
	auto fxk = f(xk);
	return xk - (fxk * fxk) / (f(xk + fxk) - fxk);
}

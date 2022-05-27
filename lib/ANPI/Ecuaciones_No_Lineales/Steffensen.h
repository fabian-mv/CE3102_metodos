#ifndef STEFFENSEN_H
#define STEFFENSEN_H

#include <cmath>
#include <cstdio>

extern "C"
{
	typedef long double Ld;

	typedef struct Solucion
	{
		Ld x;
		Ld error;
		int k;
	} Solucion;
	Solucion steffensen(Ld (*f)(Ld), Ld x0, Ld tol, int itermax);
}

#endif
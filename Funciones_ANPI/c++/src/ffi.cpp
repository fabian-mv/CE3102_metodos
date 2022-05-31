#include "anpi/ffi.h"
#include "anpi/sistema.hpp"

extern "C" sistema *new_sistema(double *a, int filas, int columnas, double *b, int longitud)
{
	return new sistema
	{
		arma::mat(a, filas, columnas, false, true),
		arma::vec(b, false, true)
	};
}

extern "C" void free_sistema(sistema *sistema_)
{
	delete sistema_;
}

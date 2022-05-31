#ifndef ANPI_SISTEMA_HPP
#define ANPI_SISTEMA_HPP

#include <armadillo>

#include "anpi/ffi.h"

struct sistema
{
	arma::mat a;
	arma::vec b;
};

#endif

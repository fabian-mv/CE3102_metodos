#ifndef ANPI_SISTEMA_HPP
#define ANPI_SISTEMA_HPP

#include <armadillo>

#include "anpi/ffi.h"

/**
 *  Struct que define una matriz aumentada.
 *  
 *  El conjunto constituido por una matriz A y el vector b, comunmente
 *  utilizados de la siguiente manera:
 *
 *  `Ax = b`
 *
 *  Donde `A` es una matriz que representa el sistema de ecuaciones, `x` es
 *  un vector que contiene las variables del sistema y `b` es el vector solución
 *  del sistema.
 *
 *  a: Matriz que representa un sistema de ecuaciones
 *  b: Vector solución del sistema
 */
struct sistema
{
	arma::mat a;
	arma::vec b;
	arma::vec solucion;
};

#endif

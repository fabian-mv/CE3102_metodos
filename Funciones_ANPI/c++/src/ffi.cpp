#include "anpi/ffi.h"
#include "anpi/sistema.hpp"

/**
 * Exporta la función new_sistema para que Haskell la pueda utilizar.
 *
 * Extern "C" hace que el nombre de una función en C++ no sea "mangled" por el
 * compilador. Esto hace que Haskell pueda utilizar la función a través de un
 * header que contiene la declaración de la función.
 * 
 * La razón por la cual estas funciones son necesarias es porque las matrices
 * y vectores de Haskell son diferentes a los de C++. Además de esto, C++
 * requiere interacción con la memoria. Estas funciones permiten transicionar
 * entre ambos lenguajes fácilmente.
 */
extern "C" sistema *new_sistema(double *a, int filas, int columnas, double *b, int longitud)
{
	return new sistema
	{
		arma::mat(a, filas, columnas, false, true),
		arma::vec(b, false, true)
	};
}

/**
 * Exporta la función free_sistema para que Haskell la pueda utilizar.
 *
 * Extern "C" hace que el nombre de una función en C++ no sea "mangled" por el
 * compilador. Esto hace que Haskell pueda utilizar la función a través de un
 * header que contiene la declaración de la función.
 * 
 * La razón por la cual estas funciones son necesarias es porque las matrices
 * y vectores de Haskell son diferentes a los de C++. Además de esto, C++
 * requiere interacción con la memoria. Estas funciones permiten transicionar
 * entre ambos lenguajes fácilmente.
 */
extern "C" void free_sistema(sistema *sistema_)
{
	delete sistema_;
}

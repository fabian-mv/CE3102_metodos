#include <armadillo>

#include "anpi/ffi.h"
#include "anpi/thomas.hpp"
#include "anpi/sistema.hpp"

using namespace arma;

/**
 * Exporta la función thomas_sistema para que Haskell la pueda utilizar.
 *
 * Extern "C" hace que el nombre de una función en C++ no sea "mangled" por el
 * compilador. Esto hace que Haskell pueda utilizar la función a través de un
 * header que contiene la declaración de la función.
 * 
 */
extern "C" double *thomas_sistema(sistema *sistema_)
{
	sistema_->solucion = thomas(sistema_->a, sistema_->b);
	return sistema_->solucion.memptr();
}


 /*
  * Aplica método iterativo de Thomas (ver comentario en header).
  * A diferencia de otros métodos, este no utiliza una condición de parada,
  * 
  */
vec thomas(const mat &A, const vec &d){

    int n = A.n_rows;
    vec zero = {0};

    vec a = A.diag(-1);
    a = join_cols(zero,a);

    vec b = A.diag();

    vec c = A.diag(1);
    c = join_cols(c,zero);

    vec p = vec(n-1);
    vec q = vec(n);
    vec x = vec(n);

    p(0) = c(1)/b(1);

    // cálculo del vector p
    // p_i = c_i/b_i                  si i = 1
    //     = c_i/b_i - p_{i-1} * a_i  si i >= 2,3, ...,n-1
    for (int i = 1; i < n-1; i++)
    {
        p(i) = c(i)/(b(i) - p(i-1) * a(i) );
    }

    q(0) = d(0)/b(0);

    // cálculo del vector q
    // q_i = d_i/b_i                                  si i = 1
    //     = d_i - q_{i-1} * a_i / b_i - p_{i-1} a_i  si i >= 2,3, ...,n-1
    for (int i = 1; i < n; i++)
    {
        q(i) = (d(i)-q(i-1)*a(i))/(b(i)-p(i-1)*a(i));
    }

    x(n-1) = q(n-1);

    // cálculo del vector x_i a partir de los vectores q y p
    // x_i = q_i - p_i * x_{i+1}
    for (int i = n-2; i >= 0; i--)
    {
        x(i) = q(i) - p(i)* x(i+1);
    }

    return x;
}


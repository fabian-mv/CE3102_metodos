#include <armadillo>

#include "anpi/ffi.h"
#include "anpi/sistema.hpp"
#include "anpi/gradiente_conjugado.hpp"

using namespace arma;


/**
 * Exporta la función gradiente_conjugado_sistema para que Haskell la pueda utilizar.
 *
 * Extern "C" hace que el nombre de una función en C++ no sea "mangled" por el
 * compilador. Esto hace que Haskell pueda utilizar la función a través de un
 * header que contiene la declaración de la función.
 * 
 */
extern "C" double *gradiente_conjugado_sistema(sistema *sistema_)
{
	sistema_->solucion = gradiente_conjugado(sistema_->a, sistema_->b);
	return sistema_->solucion.memptr();
}

 /*
  * Aplica método iterativo de Gradiente Conjugado (ver comentario en header).
  * Se detiene cuando se cumple la condición de error o se alcanza el límite
  * de iteraciones.
  */
vec gradiente_conjugado(const mat &A, const vec &b){

    int n = A.n_rows;

    vec xk = vec(n);

    for (int i = 0; i < n; i++)
    {
        // Aproximación actual del vector rk
        vec rk = b - A*xk;

        // coeficiente alfa, utilizado en el cálculo de xk
        mat alfa_k = ((rk.t() * rk) / (rk.t() * A * rk));

        // cálculo del siguiente xk
        xk = xk + (alfa_k(0,0)*rk);

        // Se calcula el error aplicando la norma dos a xk
        // si el error es menor a 1e-6, se detiene la iteración
        if (norm(rk) < 1e-6){
            break;
        }
    }

    return xk;
}

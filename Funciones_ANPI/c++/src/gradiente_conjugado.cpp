#include <armadillo>

#include "anpi/ffi.h"
#include "anpi/sistema.hpp"
#include "anpi/gradiente_conjugado.hpp"

using namespace arma;

extern "C" double *gradiente_conjugado_sistema(sistema *sistema_)
{
	sistema_->solucion = gradiente_conjugado(sistema_->a, sistema_->b);
	return sistema_->solucion.memptr();
}

vec gradiente_conjugado(const mat &A, const vec &b){

    int n = A.n_rows;

    vec xk = vec(n);

    for (int i = 0; i < n; i++)
    {
        
        vec rk = b - A*xk;

        mat alfa_k = ((rk.t() * rk) / (rk.t() * A * rk));

        xk = xk + (alfa_k(0,0)*rk);

        if (norm(rk) < 1e-6){
            break;
        }
    }

    return xk;
}

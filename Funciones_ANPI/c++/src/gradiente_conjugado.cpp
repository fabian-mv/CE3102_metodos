#include <armadillo>

#include "anpi/gradiente_conjugado.hpp"

using namespace arma;

vec gradiente_conjugado(mat A, vec b){

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

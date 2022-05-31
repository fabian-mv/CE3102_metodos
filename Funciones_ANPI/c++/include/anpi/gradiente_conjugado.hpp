#ifndef ANPI_GRADIENTE_CONJUGADO_HPP
#define ANPI_GRADIENTE_CONJUGADO_HPP

#include <armadillo>

/**
 * El método de Gradiente Conjugado consiste en lo siguiente:
 * 
 * input: A, x_0, b, error, interMax
 * k = 0
 * se repite:
 *   r_k = b - A x_k
 *   a_k = r_k^T r_k / t_K^T A r_k
 *   x_i+1 = x_k + a_k * r_k
 *   k = k + 1
 *   se detiene cuando
 *     k = iterMax
 *     |r_k| < error
 *
 * @param A Matriz que representa al sistema de ecuaciones
 * @param b Solución al sistema de ecuaciones
 * @return vec Vector solución
 */
arma::vec gradiente_conjugado(arma::mat A, arma::vec b);

#endif

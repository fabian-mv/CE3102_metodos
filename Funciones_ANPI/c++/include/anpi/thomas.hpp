#ifndef ANPI_THOMAS_HPP
#define ANPI_THOMAS_HPP

#include <armadillo>

/**
 * El método de Thomas sirve para resolver matrices tridiagonales.
 * Utiliza factorización LU y resuelve la matriz L y la U por separado.
 * 
 * Para esto, se definen los vectores p y q:
 * 
 * p_i = c_i/b_i                si i = 1
 *     = c_i/b_i - p_i-1 * a_i  si i >= 2,3, ...,n-1
 * 
 * q_i = d_i/b_i                              si i = 1
 *     = d_i - q_i-1 * a_i / b_i - p_i-1 a_i  si i >= 2,3, ...,n-1
 * 
 * La solución del sistema se obtiene con:
 * x_n = q_n
 * x_i = q_i - p_i * x_i+1 
 * 
 * para i = n-1, n-2, ..., 1
 * 
 * @param A Matriz que representa al sistema de ecuaciones
 * @param d Solución al sistema de ecuaciones
 * @return vec Vector solución
 */
arma::vec thomas(arma::mat A, arma::vec d);

#endif

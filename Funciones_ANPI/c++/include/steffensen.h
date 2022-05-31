#ifndef STEFFENSEN_H
#define STEFFENSEN_H

#ifdef __cplusplus
extern "C"
{
#endif

    /**
     * Cálculo del error.
     *
     * Para obtener el error, se evalúa la función con el valor actual de la 
     * aproximación y se compara el valor absoluto del resultado con cero.
     *
     */
    double err_steffensen(double (*f)(double), double xk);
    
    /**
     * Calcula el valor del xk de la siguiente iteración
     *
     * Para obtener la siguiente iteración se aplica el criterio del método
     * iterativo de Steffensen visto en clase.
     *
     */
    double iter_steffensen(double (*f)(double), double xk);

#ifdef __cplusplus
}
#endif

#endif

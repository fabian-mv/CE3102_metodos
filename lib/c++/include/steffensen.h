#ifndef STEFFENSEN_H
#define STEFFENSEN_H

#ifdef __cplusplus
extern "C"
{
#endif

    /**
     * Cálculo del error
     */
    double err_steffensen(double (*f)(double), double xk);
    
    /**
     * Calcula el valor del xk de la siguiente iteración
     */
    double iter_steffensen(double (*f)(double), double xk);

#ifdef __cplusplus
}
#endif

#endif

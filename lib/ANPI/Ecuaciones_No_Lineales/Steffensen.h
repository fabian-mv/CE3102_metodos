#ifndef STEFFENSEN_H
#define STEFFENSEN_H

#ifdef __cplusplus
extern "C"
{
#endif

    double err_steffensen(double (*f)(double), double xk);
    double iter_steffensen(double (*f)(double), double xk);

#ifdef __cplusplus
}
#endif

#endif

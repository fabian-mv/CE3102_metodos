#ifndef STEFFENSEN_H
#define STEFFENSEN_H

#ifdef __cplusplus
extern "C"
{
#endif

    double err_steffensen(double (*f)(void*, double), void *arg, double xk);
    double iter_steffensen(double (*f)(void*, double), void *arg, double xk);

#ifdef __cplusplus
}
#endif

#endif

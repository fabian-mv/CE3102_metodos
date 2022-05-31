#ifndef ANPI_FFI_H
#define ANPI_FFI_H

#ifdef __cplusplus
extern "C"
{
#endif

struct sistema *new_sistema(double *a, int filas, int columnas, double *b, int longitud);
void            free_sistema(struct sistema *sistema);

#ifdef __cplusplus
}
#endif

#endif

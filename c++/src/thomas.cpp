#include "thomas.h"

vec thomas(mat A, vec d){

    int n = A.n_rows;
    vec zero = {0};

    vec a = A.diag(-1);
    a = join_cols(zero,a);

    vec b = A.diag();

    vec c = A.diag(1);
    c = join_cols(c,zero);

    vec p = vec(n-1);
    vec q = vec(n);
    vec x = vec(n);

    p(0) = c(1)/b(1);

    for (int i = 1; i < n-1; i++)
    {
        p(i) = c(i)/(b(i) - p(i-1) * a(i) );
    }

    q(0) = d(0)/b(0);

    for (int i = 1; i < n; i++)
    {
        q(i) = (d(i)-q(i-1)*a(i))/(b(i)-p(i-1)*a(i));
    }

    x(n-1) = q(n-1);

    for (int i = n-2; i >= 0; i--)
    {
        x(i) = q(i) - p(i)* x(i+1);
    }

    return x;
}


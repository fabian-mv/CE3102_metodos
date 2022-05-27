#include "Ecuaciones_No_Lineales/Steffensen.h"

Ld problema_ejemplo(Ld vgs)
{
	// ejemplo 6.8 Razavi
	Ld rd{5e3};
	Ld vdd{1.8};
	Ld w{2.0};
	Ld l{0.18};

	// valores adicionales
	Ld vth{0.4};
	Ld un_cox{100e-6};
	return vdd - (rd / 2.0) * un_cox * (w / l) * (vgs - vth) * (vgs - vth) - vgs + vth;
}

int main()
{
	Solucion res = steffensen(problema_ejemplo, 0.6, 1e-5, 1000);
	printf("Resultado: %.15LF \n -- Error: %.15LF \n -- iteraciones: %d \n", res.x, res.error, res.k);
}
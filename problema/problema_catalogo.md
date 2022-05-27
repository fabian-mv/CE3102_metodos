---
title:
  Instituto Tecnológico de Costa Rica\endgraf\bigskip \endgraf\bigskip\bigskip\
  Programación de Métodos Numéricos $:$ Aplicación de método de Steffenson para resolución de un problema de ingeniería \endgraf\bigskip\bigskip\bigskip\bigskip
author:
  - Alejandro Soto Chacón, carné 2019008164
  - Fabian Montero Villalobos, carné 2016121558
  - José Chavarría Madriz, carné 2019067306
  - José Morales Vargas, carné 2019024270
date: \bigskip\bigskip\bigskip\bigskip Área Académica de\endgraf Ingeniería en Computadores \endgraf\bigskip\bigskip\ Análisis Numérico Para Ingeniería \endgraf  (CE3102) \endgraf\bigskip\bigskip Profesor Juan Pablo Soto Quirós \endgraf\vfill  Semestre I 2022
header-includes:
  - \setlength\parindent{24pt}
  - \usepackage{url}
  - \usepackage{float}
  - \floatplacement{figure}{H}
lang: es-ES
papersize: letter
classoption: fleqn
geometry: margin=1in
fontsize: 12pt
fontfamily: sans
linestretch: 1.15
bibliography: bibliografia.bib
csl: ieee.csl
nocite: |
...

\maketitle
\thispagestyle{empty}
\clearpage
\tableofcontents
\pagenumbering{roman}
\clearpage
\pagenumbering{arabic}
\setcounter{page}{1}

# Aplicación del método de Steffensen para la resolución de un problema de ingeniería

## Contexto del problema

El problema consiste en el análisis de un circuito simple compuesto por un MOSFET(Metal Oxide Semiconductor Field Effect Transistor) y una resistencia. Un transistor MOS es un dispositivo semiconductor multifuncional que es usado en una variedad de aplicaciones, aunque generalmente se estudia en el contexto de diseño de amplificadores de señales. 

![Estructura de un MOSFET \label{mosfet}. Obtenido de [@razavi].](mosfet.png){width=70%}

En la figura \ref{mosfet} se puede ver la estructura básica de un MOSFET. En teŕminos simples, el dispositivo consiste de dos terminales fuente (source) y  drenaje (drain) cuya conexión es controlada por una tercera terminal denominada compuerta (Gate). Algunos valores que caracterizan el comportamiento general de un MOSFET son:

- $V_{TH}$: También denominada tensión térmica. Relacionada a comportamiento de los portadores de carga del semiconductor.
- $\mu_{n}$: Movilidad de electrones en el sustrato.
- $C_{ox}$: Capacitancia por unidad de área del óxido de la compuerta.
- $W$: Ancho de la compuerta (distancia que separa drenador y fuente).
- $L$: Largo de la compuerta.

![Regiones de operación de un MOSFET \label{regiones}. Obtenido de [@razavi]](mosfet_operacion.png){width=70%}

Un transistor MOS posee distintas regiones de operación determinadas por los valores de tensión encontrados en las terminales del dispositivo. Cada región de operación significa un comportamiento distinto del dispositivo, en particular, la ecuación que describe la corriente de drenaje. En la región conocida como región de triodo, el transistor se comporta similar a una resistencia, mientras que en la región de saturación se comporta como una fuente de corriente controlada por tensión con la corriente en el drenaje siendo descrita por la siguiente fórmula:

$$I_D = \frac{1}{2}\mu_n C_{ox} \frac{W}{L}(V_{GS} - V_{TH})^2$$

La figura \ref{regiones} muestra que el borde entre ambas regiones se encuentra cuando $V_{GS} - V_{TH}$ tienen el mismo valor que $V_{DS}$. $V_{GS}$ se refiere a la diferencia de tensión entre la compuerta y la fuente, y $V_{DS} a la diferencia entre drenador y fuente$.  

## El problema a resolver

Se busca encontrar un valor límite de $V_{GS}$ para que el transistor MOS permanezca en estado de saturación. Además de los datos provistos en la figura \ref{fig-circuito}, se señala que el valor de $\mu_nC_{ox} = 100 \mu\text{A/V}^2$ y $V_{TH} = 0.4$ V. 

![Circuito MOS simple. Obtenido de [@razavi] \label{fig-circuito}.](diagrama_problema.png){width=40%}

Al analizar el circuito se puede observar que:

$$V_{DS} = V_{DD} - R_DI_D$$

Dado que para cumplir la condición límite buscada $V_{DS} = V_{GS} - V_{TH}$, se obtiene la siguiente ecuación:

$$V_{GS} - V_{TH} = V_{DD} - \frac{R_D}{2}\mu_n c_{ox} \frac{W}{L}(V_{GS} - V_{TH})^2$$

$$0 = V_{DD} - \frac{R_D}{2}\mu_n c_{ox} \frac{W}{L}(V_{GS} - V_{TH})^2 - V_{GS} + V_{TH}$$

De lo anterior se puede notar que la incógnita es $V_{GS}$ y la expresión representa una ecuación de segundo orden. 

## Solución computacional del problema

Para la solución del problema se hace uso del método de Steffensen, el cual se puede definir por la fórmula iterativa:

$$
\begin{cases}
x_0 \\
x_{n+1} = x_n - \frac{f(x_n)^2}{f(x_n + f(x_n)) - f(x_n)x_n}
\end{cases}
$$

```C++
#include <cmath>
#include <cstdio>

typedef long double Ld;

typedef struct Solucion
{
	Ld x;
	Ld error;
	int k;
} Solucion;

Solucion steffensen(Ld (*f)(Ld), Ld x0, Ld tol, int itermax)
{
    Ld xk{x0};
    Ld error{0};
    Ld fxk{0};
    int k{itermax};
    for (int i = 0; i < itermax; i++)
    {
        fxk = f(xk);
        xk = xk - (fxk * fxk) / (f(xk + fxk) - fxk);
        error = std::fabs(f(xk));
        if (error < tol)
        {
            k = i;
            break;
        }
    }
    return Solucion{
        .x = xk,
        .error = error,
        .k = k};
}

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
	return vdd-(rd/2.0)*un_cox*(w/l)*(vgs-vth)*(vgs-vth)-vgs+vth;
}

int main()
{
	Solucion res = steffensen(problema_ejemplo, 0.6, 1e-5, 1000);
	printf("Resultado: %.15LF \n", res.x);
    printf("-- Error: %.15LF\n", res.error);
    printf("-- iteraciones: %d \n", res.k);
    return 0;
}

```

El resultado que el programa anterior imprime al ser ejecutado en consola es:


```Shell
Resultado: 1.044863596655522 
-- Error: 0.000000130312545
-- iteraciones: 4 
```

La cual es el valor correcto para $V_{GS}$ de manera que el transistor pueda permanecer en saturación.

# Referencias

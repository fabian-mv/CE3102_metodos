---
title:
  Instituto Tecnológico de Costa Rica\endgraf\bigskip \endgraf\bigskip\bigskip\
  Programación de Métodos Numéricos $:$ Aplicación de método de Steffensen para resolución de un problema de ingeniería \endgraf\bigskip\bigskip\bigskip\bigskip
author:
  - Alejandro Soto Chacón, carné 2019008164
  - Fabian Montero Villalobos, carné 2016121558
  - José Chavarría Madriz, carné 2019067306
  - José Morales Vargas, carné 2019024270
date: \bigskip\bigskip\bigskip\bigskip Área Académica de\endgraf Ingeniería en Computadores \endgraf\bigskip\bigskip\ Análisis Numérico Para Ingeniería \endgraf  (CE3102) \endgraf\bigskip\bigskip Profesor Juan Pablo Soto Quirós \endgraf\vfill  Semestre I 2022
header-includes:
  - \setlength\parindent{24pt}
lang: es-ES
papersize: letter
geometry: margin=1in
fontsize: 12pt
fontfamily: sans
monofont: "Hack"
linestretch: 1.15
bibliography: bibliografia.bib
csl: ieee.csl
nocite: | 
    @codetobuy
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

En la figura \ref{mosfet} se puede ver la estructura básica de un MOSFET. En términos simples, el dispositivo consiste de dos terminales fuente (source) y  drenaje (drain) cuya conexión es controlada por una tercera terminal denominada compuerta (Gate). Algunos valores que caracterizan el comportamiento general de un MOSFET son:

- $V_{TH}$: También denominada tensión térmica. Relacionada a comportamiento de los portadores de carga del semiconductor.
- $\mu_{n}$: Movilidad de electrones en el sustrato.
- $C_{ox}$: Capacitancia por unidad de área del óxido de la compuerta.
- $W$: Ancho de la compuerta (distancia que separa drenaje y fuente).
- $L$: Largo de la compuerta.

![Regiones de operación de un MOSFET \label{regiones}. Obtenido de [@razavi]](mosfet_operacion.png){width=70%}

Un transistor MOS posee distintas regiones de operación determinadas por los valores de tensión encontrados en las terminales del dispositivo. Cada región de operación significa un comportamiento distinto del dispositivo, en particular, la ecuación que describe la corriente de drenaje. En la región conocida como región de triodo, el transistor se comporta similar a una resistencia, mientras que en la región de saturación se comporta como una fuente de corriente controlada por tensión con la corriente en el drenaje siendo descrita por la siguiente fórmula:

$$I_D = \frac{1}{2}\mu_n C_{ox} \frac{W}{L}(V_{GS} - V_{TH})^2$$

La figura \ref{regiones} muestra que el borde entre ambas regiones se encuentra cuando $V_{GS} - V_{TH}$ tienen el mismo valor que $V_{DS}$. $V_{GS}$ se refiere a la diferencia de tensión entre la compuerta y la fuente, y $V_{DS} a la diferencia entre drenaje y fuente$.  

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

Una posible implementación computacional en `C++` es la mostrada a continuación:

\scriptsize

---

~~~ { .cpp caption="Implementación computacional en C++ del método de Steffensen y resolución del problema planteado"}
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
    printf("Error: %.15LF\n", res.error);
    printf("iteraciones: %d \n", res.k);
    return 0;
}
~~~
---

\normalsize

El resultado que el programa anterior imprime al ser ejecutado en consola es:


\scriptsize

~~~ { .bash}
Resultado: 1.044863596655522 
Error: 0.000000130312545
iteraciones: 4 
~~~

\normalsize

La cual es el valor correcto para $V_{GS}$ de manera que el transistor pueda permanecer en saturación.

## Solución con acercaminto híbrido `C++` y `Haskell`

En el contexto del trabajo desarrollado, debido a que se quería poder tener acceso a todos los métodos implementados desde una biblioteca de `Haskell`, se realizaron algunas adaptaciones para integrar el código `C++`. En primer lugar, al igual que para otro métodos, la lógica de la iteración se relega a `Haskell`, mientras que las fórmulas de error y valor de siguiente iteración se mantienen en `C++`:

\scriptsize

---

~~~ { .cpp caption="Stenffensen.h"}
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
~~~

---

\normalsize

\scriptsize

---

~~~ { .cpp caption="Stenffensen.cpp"}
#include <cmath>
#include <cstdio>

#include "Steffensen.h"

extern "C"
{
    double err_steffensen(double (*f)(double), double xk)
    {
        return std::fabs(f(xk));
    }

    double iter_steffensen(double (*f)(double), double xk)
    {
        auto fxk = f(xk);
        return xk - (fxk * fxk) / (f(xk + fxk) - fxk);
    }
}
~~~

---

\normalsize

Como puede observarse, en la parte de `C++` se hace uso de "extern", lo que permite definir una interfaz utilizable desde `Haskell`. Antes de hacer uso de la interfaz, es necesario mostrar las definiciones base que se utilizan en el programa. Estas definiciones base se muestran en los archivos `Base.hs`, uno que se encuentra a nivel de biblioteca y otro que es dedicado para los métodos de resolución de ecuaciones no lineales en el contexto del trabajo en general. 

\scriptsize

---

~~~ { .haskell caption="Base.hs"}
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

module ANPI.Base
( Iteracion (..)
, Param (..)
, Solucion (..)
, denom
, solucion
) where

import Numeric.LinearAlgebra

data Param o = Param
  { objetivo  :: o
  , tol       :: Double
  , iterMax   :: Int
  }


data Iteracion s = Iteracion
  { k        :: Int
  , aprox_k  :: s
  , err      :: Double
  } deriving Show


class Solucion o s | s -> o where
  error_k :: o -> s -> Double
  siguiente :: o -> s -> s


denom :: Double -> Double
denom x =
  if   abs x > peps
  then x
  else error "Denominador se anula."


solucion :: Solucion o s => Param o -> s -> Iteracion s
solucion param aprox_0 = head . dropWhile (not . parar) . iterate sucesor $
  Iteracion { k = 0, aprox_k = aprox_0, err = error_k objetivo' aprox_0 } where

  parar iter = k iter >= iterMax param || error_k objetivo' (aprox_k iter) < tol param
  
  sucesor iter =
    let aprox_km1 = siguiente objetivo' (aprox_k iter)
    in  Iteracion
        { k       = k iter + 1
        , aprox_k = aprox_km1
        , err     = error_k objetivo' aprox_km1
        }

  objetivo' = objetivo param
~~~

\normalsize

---

\scriptsize

---

~~~ { .haskell caption="Ecuaciones_No_Lineales/Base.hs"}
module ANPI.Ecuaciones_No_Lineales.Base
( Criterio
, Derivable (..)
) where

import Numeric.LinearAlgebra

type Criterio = Double -> Double

data Derivable = Derivable
  { f  :: Criterio
  , f' :: Criterio
  }
~~~

---

\normalsize

Posteriormente, se agrega el código necesario para "conectarse" con el lado de `C++` desde Haskell e integrar el método de Stenffensen a la biblioteca como un todo. 

\scriptsize

---

~~~ { .haskell caption="Ecuaciones_No_Lineales/Steffensen.hs"}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses, CApiFFI #-}

module ANPI.Ecuaciones_No_Lineales.Steffensen
( Steffensen (..)
, withForeign
) where 

import Foreign.C.Types
import Foreign.Ptr

import ANPI.Base
import ANPI.Ecuaciones_No_Lineales.Base

foreign import capi "Steffensen.h err_steffensen" errSteffensen
  :: FunPtr Criterio -> CDouble -> CDouble

foreign import capi "Steffensen.h iter_steffensen" iterSteffensen
  :: FunPtr Criterio -> CDouble -> CDouble

data Steffensen = Steffensen 
  { x_k :: Double
  } deriving Show

withForeign :: Criterio -> (FunPtr Criterio -> IO a) -> IO a
withForeign criterio operacion = do
  ptr <- wrapCriterio criterio
  salida <- operacion ptr
  freeHaskellFunPtr ptr
  return salida

instance Solucion (FunPtr Criterio) Steffensen where
  error_k criterio aprox =
    let CDouble valor = errSteffensen criterio (CDouble (x_k aprox))
    in  valor

  siguiente criterio aprox =
    let CDouble valor = iterSteffensen criterio (CDouble (x_k aprox))
    in  Steffensen { x_k = valor }

foreign import ccall "wrapper" wrapCriterio :: Criterio -> IO (FunPtr Criterio)
~~~

---

\normalsize

Finalmente, podemos hacer uso del método implementado desde `Haskell`.

\scriptsize

---

~~~ {.haskell caption="RunExample.hs"}
import ANPI.Base
import ANPI.Ecuaciones_No_Lineales.Base
import ANPI.Ecuaciones_No_Lineales.Steffensen

problemaEjemplo :: Double -> Double
problemaEjemplo vgs =
  vdd - (rd / 2) * un_cox * (w / l) * (vgs - vth)^2 - vgs + vth
  where
    rd  = 5e3
    vdd = 1.8
    w   = 2.0
    l   = 0.18
    vth    = 0.4
    un_cox = 100e-6

main :: IO ()
main = withForeign problemaEjemplo $ \f -> print $ solucion (param f) x_0 where
  x_0     = Steffensen { x_k = 0.6 }
  param f = Param
    { tol      = 1e-5
    , iterMax  = 1000
    , objetivo = f
    }
~~~

---

\normalsize

El resultado obtenido es el mismo, aunque ahora el método está disponible como parte de una biblioteca completa:

\scriptsize 

~~~ { .bash}
Iteracion {k = 5, aprox_k = Steffensen {x_k = 1.0448635966555222}, err = 1.3031254464390685e-7}
~~~

\normalsize

# Referencias

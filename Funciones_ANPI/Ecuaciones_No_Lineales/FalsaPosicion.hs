{-# LANGUAGE TypeSynonymInstances, FlexibleInstances
  , MultiParamTypeClasses, DeriveGeneric, DeriveAnyClass #-}

{-
  Este módulo contiene la funcionalidad del método iterativo Falsa Posición.

  Utiliza los módulos Funciones_ANPI.Base y Funciones_ANPI.Ecuaciones_No_Lineales.Base en su
  funcionamiento.
-}


{-
  Exportación del tipo FalsaPosicion para que pueda ser utilizado afuera
  del módulo.
-}
module Funciones_ANPI.Ecuaciones_No_Lineales.FalsaPosicion
( FalsaPosicion (..)
) where

import GHC.Generics (Generic)

import Control.DeepSeq

import Funciones_ANPI.Base
import Funciones_ANPI.Ecuaciones_No_Lineales.Base


{-
  Algebraic Data Type que describe el estado de cada iteración del método
  iterarivo de la FalsaPosicion.

  `deriving Show` hace que este ADT sea imprimible en la terminal.
  `deriving NFData` hace que este ADT pueda ser expandido de manera eager,
  lo cual hace que los cálculos de la benchmark sean precisos.
  `deriving Generic` es requerido para usar NFData

  Esta definición es así de sencilla gracias a que la mayoría del boiler plate
  necesario para la ejecución del método está definido en Funciones_ANPI.Base y en 
  Funciones_ANPI.Ecuaciones_No_Lineales.Base.

  a: límite izquiero
  b: límite derecho
-}
data FalsaPosicion = FalsaPosicion
  { a :: Double
  , b :: Double
  } deriving (Show, Generic, NFData)


{-
  Se instancia una Solución utilizando el Criterio de la función
  de Falsa Posicion.

  Se calcula el error en la iteración actual comparando el valor absoluto de
  de la función evaluada en x_k.

  Se calcula la siguiente iteración usando el método de la Falsa Posicion.
-}
instance Solucion Criterio FalsaPosicion where
  error_k   criterio aprox = abs . criterio . x_k criterio $ aprox
  siguiente criterio aprox = FalsaPosicion
    -- Para calcular la siguiente iteración, se aplica la definición del método
    -- de la Falsa Posicion visto en clase.
    -- Esto consiste principalmente en la aplicación del teorema de Bolzano,
    -- como se muestra a continuación. Además, difiere del método de la
    -- Bisección en que el cáclulo de x_k se realiza utilizando una fórmula
    -- matemática diferente.
    { a = if bolzano then a aprox else x
    , b = if bolzano then x       else b aprox
    } where
    -- El bloque `where` añade estructura al programa y permite evitar la
    -- repetición de código y aumenta claridad del programa. Aunque esta
    -- sintaxis puede parecer extraña, es análoga a la palabra "donde", la cual
    -- se usa comúnmente en el lenguaje matemático. Por ejemplo:
    --     x + 0 = 1,
    --       donde x = 1
    -- 
    -- Como se observa, la palabra clave `where` inicia un bloque en el cual
    -- se declaran ecuaciones que se reemplazan en el código que está arriba
    -- del `where`.
    
    -- Cálculo de x_k
    x       = x_k criterio aprox
    -- Dado un criterio y dos calores, determina si se cumple el teorema de Bolzano 
    bolzano = (signum . criterio . a) aprox /= signum (criterio x)

x_k criterio aprox = b' - (b' - a') * criterio b' / denom d where
  d  = criterio b' - criterio a'
  b' = b aprox
  a' = a aprox

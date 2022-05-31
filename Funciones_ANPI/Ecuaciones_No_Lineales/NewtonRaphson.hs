{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, DeriveGeneric, DeriveAnyClass #-}

{-
  Este módulo contiene la funcionalidad del método iterativo Newton Raphson.

  Utiliza los módulos Funciones_ANPI.Base y Funciones_ANPI.Ecuaciones_No_Lineales.Base en su
  funcionamiento.
-}


{-
  Exportación del tipo NewtonRaphson para que pueda ser utilizado afuera
  del módulo.
-}
module Funciones_ANPI.Ecuaciones_No_Lineales.NewtonRaphson
( NewtonRaphson (..)
) where

import GHC.Generics (Generic)

import Control.DeepSeq

import Funciones_ANPI.Base
import Funciones_ANPI.Ecuaciones_No_Lineales.Base

{-
  Tipo que describe el estado de cada iteración del método iterarivo de
  Newton Raphson.

  `deriving Show` hace que este ADT sea imprimible en la terminal.
  `deriving NFData` hace que este ADT pueda ser expandido de manera eager,
  lo cual hace que los cálculos de la benchmark sean precisos.
  `deriving Generic` es requerido para usar NFData

  Esta definición es así de sencilla gracias a que la mayoría del boiler plate
  necesario para la ejecución del método está definido en Funciones_ANPI.Base y en 
  Funciones_ANPI.Ecuaciones_No_Lineales.Base.

  x_k: Valor inicial de la aproximación.
-}
newtype NewtonRaphson = NewtonRaphson
  { x_k :: Double
  } deriving (Show, Generic, NFData)


{-
  Se instancia una Solución utilizando el Criterio de la función
  de Newton Raphson.

  Se calcula el error en la iteración actual comparando el valor absoluto de
  de la función evaluada en x_k.

  Se calcula la siguiente iteración usando el método de Newton Raphson.
-}
instance Solucion Derivable NewtonRaphson where
  error_k   func aprox = abs . f func . x_k $ aprox
  siguiente func aprox = NewtonRaphson
    -- Para calcular la siguiente iteración, se aplica la definición del método
    -- de Newton Raphson visto en clase.
    -- Esto consiste en la aplicación del criterio de este método.
    -- Nótese que se utiliza la función denom para evitar la división entre cero.
    { x_k = x_km1 - f func x_km1 / denom d } where
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
    
    d     = f' func x_km1
    x_km1 = x_k aprox

{-# LANGUAGE TypeSynonymInstances, FlexibleInstances
  , MultiParamTypeClasses, DeriveGeneric, DeriveAnyClass #-}

{-
  Este módulo contiene la funcionalidad del método iterativo Bisección.

  Utiliza los módulos ANPI.Base y ANPI.Ecuaciones_No_Lineales.Base en su
  funcionamiento.
-}


{-
  Exportación del tipo Bisección para que pueda ser utilizado afuera del módulo.
-}
module ANPI.Ecuaciones_No_Lineales.Biseccion
( Biseccion (..)
) where 

import GHC.Generics (Generic)

import Control.DeepSeq

import ANPI.Base
import ANPI.Ecuaciones_No_Lineales.Base


{-
  Algebraic Data Type que define el estado inicial del método iterarivo de la
  Bisección.

  `deriving Show` hace que este ADT sea imprimible en la terminal.

  Esta definición es así de sencilla gracias a que la mayoría del boiler plate
  necesario para la ejecución del método está definido en ANPI.Base y en 
  ANPI.Ecuaciones_No_Lineales.Base.

  a: límite izquiero
  b: límite derecho
-}
data Biseccion = Biseccion
  { a :: Double
  , b :: Double
  } deriving (Show, Generic, NFData)


{-
  Se instancia una Solución utilizando el Criterio de la función de Bisección.

  Se calcula el error en la iteración actual a partir de un pivote usando
  la función x_k.

  Se calcula la siguiente iteración usando el método de la Bisección.
-}
instance Solucion Criterio Biseccion where
  error_k   criterio aprox = abs . criterio . x_k $ aprox
  siguiente criterio aprox = Biseccion
    -- Para calcular la siguiente iteración, se aplica la definición del método
    -- de la Bisección visto en clase.
    -- Esto consiste principalmente en la aplicación del teorema de Bolzano,
    -- como se muestra a continuación.
    { a = if bolzano then a aprox else x
    , b = if bolzano then x       else b aprox
    } where

    x       = x_k aprox
    -- Dado un criterio y dos calores, determina si se cumple el teorema de Bolzano 
    bolzano = (signum . criterio . a $ aprox) /= signum (criterio x)

x_k aprox = (a aprox + b aprox)/2

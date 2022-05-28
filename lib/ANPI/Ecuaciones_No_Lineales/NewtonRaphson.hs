{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, DeriveGeneric, DeriveAnyClass #-}

{-
  Este módulo contiene la funcionalidad del método iterativo Newton Raphson.

  Utiliza los módulos ANPI.Base y ANPI.Ecuaciones_No_Lineales.Base en su
  funcionamiento.
-}


{-
  Exportación del tipo NewtonRaphson para que pueda ser utilizado afuera
  del módulo.
-}
module ANPI.Ecuaciones_No_Lineales.NewtonRaphson
( NewtonRaphson (..)
) where

import GHC.Generics (Generic)

import Control.DeepSeq

import ANPI.Base
import ANPI.Ecuaciones_No_Lineales.Base

{-
  Tipo que define el estado inicial del método iterarivo de la
  Newton Raphson.

  `deriving Show` hace que este tipo sea imprimible en la terminal.

  Esta definición es así de sencilla gracias a que la mayoría del boiler plate
  necesario para la ejecución del método está definido en ANPI.Base y en 
  ANPI.Ecuaciones_No_Lineales.Base.

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

  Se calcula la siguiente iteración usando el método de la Newton Raphson.
-}
instance Solucion Derivable NewtonRaphson where
  error_k   func aprox = abs . f func . x_k $ aprox
  siguiente func aprox = NewtonRaphson
    -- Para calcular la siguiente iteración, se aplica la definición del método
    -- de la Newton Raphson visto en clase.
    -- Esto consiste en la aplicación del criterio de este método.
    -- Nótese que se utiliza la función d para evitar la división entre cero.
    { x_k = x_km1 - f func x_km1 / denom d } where
    
    d     = f' func x_km1
    x_km1 = x_k aprox

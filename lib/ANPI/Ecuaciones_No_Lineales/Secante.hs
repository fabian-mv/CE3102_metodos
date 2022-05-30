{-# LANGUAGE TypeSynonymInstances, FlexibleInstances
  , MultiParamTypeClasses, DeriveGeneric, DeriveAnyClass #-}

{-
  Este módulo contiene la funcionalidad del método iterativo Secante.

  Utiliza los módulos ANPI.Base y ANPI.Ecuaciones_No_Lineales.Base en su
  funcionamiento.
-}


{-
  Exportación del tipo Secante para que pueda ser utilizado afuera
  del módulo.
-}
module ANPI.Ecuaciones_No_Lineales.Secante
( Secante (..)
) where

import GHC.Generics (Generic)

import Control.DeepSeq

import ANPI.Base
import ANPI.Ecuaciones_No_Lineales.Base

{-
  Tipo que describe el estado de cada iteración del método iterarivo de
  la Secante.

  `deriving Show` hace que este ADT sea imprimible en la terminal.
  `deriving NFData` hace que este ADT pueda ser expandido de manera eager,
  lo cual hace que los cálculos de la benchmark sean precisos.
  `deriving Generic` es requerido para usar NFData

  Esta definición es así de sencilla gracias a que la mayoría del boiler plate
  necesario para la ejecución del método está definido en ANPI.Base y en 
  ANPI.Ecuaciones_No_Lineales.Base.

  x_k: Valor inicial de la aproximación.
  x_km1: Antesecor inicial de x_k
-}
data Secante = Secante
  { x_k   :: Double
  , x_km1 :: Double
  } deriving (Show, Generic, NFData)

{-
  Se instancia una Solución utilizando el Criterio de la función
  de la Secante.

  Se calcula el error en la iteración actual comparando el valor absoluto de
  de la función evaluada en x_k.

  Se calcula la siguiente iteración usando el método de la Secante.
-}
instance Solucion Criterio Secante where
  error_k   criterio aprox = abs . criterio . x_k $ aprox
  siguiente criterio aprox = Secante
    -- Para calcular la siguiente iteración, se aplica la definición del método
    -- de lSecante visto en clase.
    -- Esto consiste en la aplicación del criterio de este método.
    -- Nótese que se utiliza la función d para evitar la división entre cero.
    { x_k   = x_k' - (x_k' - x_km1') * criterio x_k' / denom d
    , x_km1 = x_k'
    } where

    x_k'   = x_k   aprox
    x_km1' = x_km1 aprox 
    d      = criterio x_k' - criterio x_km1'

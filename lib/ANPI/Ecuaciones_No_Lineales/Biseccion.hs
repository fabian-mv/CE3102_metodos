{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses #-}

module ANPI.Ecuaciones_No_Lineales.Biseccion (Biseccion (..))
where 

import ANPI.Base
import ANPI.Ecuaciones_No_Lineales.Base

data Biseccion = Biseccion
  { a :: Double
  , b :: Double
  } deriving Show

instance Solucion Criterio Biseccion where
  error_k   criterio aprox = abs . criterio . x_k $ aprox
  siguiente criterio aprox = Biseccion
    { a = if bolzano then a aprox else x
    , b = if bolzano then x       else b aprox
    } where

    x       = x_k aprox
    bolzano = (signum . criterio . a $ aprox) /= signum (criterio x)

x_k aprox = (a aprox + b aprox)/2
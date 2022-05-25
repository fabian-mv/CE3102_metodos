{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses #-}

module ANPI.Ecuaciones_No_Lineales.FalsaPosicion (FalsaPosicion (..))
where

import ANPI.Base

data FalsaPosicion = FalsaPosicion
  { a :: Double
  , b :: Double
  } deriving Show

instance Solucion Criterio FalsaPosicion where
  error_k   criterio aprox = abs . criterio . x_k criterio $ aprox
  siguiente criterio aprox = FalsaPosicion
    { a = if bolzano then a aprox else x
    , b = if bolzano then x       else b aprox
    } where

    x       = x_k criterio aprox
    bolzano = (signum . criterio . a $ aprox) /= signum (criterio x)

x_k criterio aprox = b' - (b' - a') * criterio b' / denom d where
  d  = criterio b' - criterio a'
  b' = b aprox
  a' = a aprox
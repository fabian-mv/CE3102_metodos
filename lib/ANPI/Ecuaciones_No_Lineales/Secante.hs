{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses #-}

module ANPI.Ecuaciones_No_Lineales.Secante (Secante (..))
where

import ANPI.Base
import Numeric.LinearAlgebra

data Secante = Secante
  { x_k   :: Double
  , x_km1 :: Double
  } deriving Show

instance Solucion Criterio Secante where
  error_k   criterio aprox = abs . criterio . x_k $ aprox
  siguiente criterio aprox = Secante
    { x_k   = x_k' - (x_k' - x_km1') * criterio x_k' / denom
    , x_km1 = x_k'
    } where

    x_k'   = x_k   aprox
    x_km1' = x_km1 aprox 
    denom = if   abs denom' > peps
            then denom'
            else error "Denominador se anula."
    denom' = criterio x_k' - criterio x_km1'
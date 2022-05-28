{-# LANGUAGE TypeSynonymInstances, FlexibleInstances
  , MultiParamTypeClasses, DeriveGeneric, DeriveAnyClass #-}

module ANPI.Ecuaciones_No_Lineales.Secante
( Secante (..)
) where

import GHC.Generics (Generic)

import Control.DeepSeq

import ANPI.Base
import ANPI.Ecuaciones_No_Lineales.Base

data Secante = Secante
  { x_k   :: Double
  , x_km1 :: Double
  } deriving (Show, Generic, NFData)

instance Solucion Criterio Secante where
  error_k   criterio aprox = abs . criterio . x_k $ aprox
  siguiente criterio aprox = Secante
    { x_k   = x_k' - (x_k' - x_km1') * criterio x_k' / denom d
    , x_km1 = x_k'
    } where

    x_k'   = x_k   aprox
    x_km1' = x_km1 aprox 
    d      = criterio x_k' - criterio x_km1'

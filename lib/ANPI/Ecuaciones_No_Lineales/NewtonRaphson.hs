{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

module ANPI.Ecuaciones_No_Lineales.NewtonRaphson
( NewtonRaphson (..)
) where

import ANPI.Base
import ANPI.Ecuaciones_No_Lineales.Base

data NewtonRaphson = NewtonRaphson
  { x_k :: Double
  } deriving Show

instance Solucion Derivable NewtonRaphson where
  error_k   func aprox = abs . f func . x_k $ aprox
  siguiente func aprox = NewtonRaphson
    { x_k = x_km1 - f func x_km1 / denom d } where
    
    d     = f' func x_km1
    x_km1 = x_k aprox
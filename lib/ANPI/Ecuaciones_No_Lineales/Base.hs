module ANPI.Ecuaciones_No_Lineales.Base
( Criterio
, Derivable (..)
, denom
) where

import Numeric.LinearAlgebra

type Criterio = Double -> Double

data Derivable = Derivable
  { f  :: Criterio
  , f' :: Criterio
  }

denom :: Criterio
denom x =
  if   abs x > peps
  then x
  else error "Denominador se anula."
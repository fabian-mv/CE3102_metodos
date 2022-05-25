module ANPI.Ecuaciones_No_Lineales.Base
( Criterio
, Derivable (..)
) where

import Numeric.LinearAlgebra

type Criterio = Double -> Double

data Derivable = Derivable
  { f  :: Criterio
  , f' :: Criterio
  }
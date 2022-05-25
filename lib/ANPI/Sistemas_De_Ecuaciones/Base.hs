module ANPI.Sistemas_De_Ecuaciones.Base
( Sistema (..)
, n
) where

import Numeric.LinearAlgebra

data Sistema = Sistema
  -- Haskell no permite llamarle "A" mayúscula
  { a :: Matrix R
  , b :: Vector R
  }

n :: Matrix a -> Int
n matriz = 
  if   rows matriz == cols matriz 
  then rows matriz
  else error "Matriz no cuadrada."
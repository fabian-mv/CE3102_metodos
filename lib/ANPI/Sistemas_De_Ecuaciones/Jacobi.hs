{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

module ANPI.Sistemas_De_Ecuaciones.Jacobi (Jacobi (..))
where

import Numeric.LinearAlgebra

import ANPI.Base
import ANPI.Sistemas_De_Ecuaciones.Base

newtype Jacobi = Jacobi
  { x_k :: Vector R
  } deriving Show

instance Solucion Sistema Jacobi where
  error_k   sistema aprox = norm_2 (a sistema #> x_k aprox - b sistema)
  siguiente sistema aprox = Jacobi
    { x_k = vector . map xi_km1 $ is
    } where

    (a', b', x_k') = (a sistema, b sistema, x_k aprox)
    is             = [0..n a' - 1]

    xi_km1 i =
      (b' ! i - sum [a' ! i ! j * x_k' ! j | j <- is, j /= i]) / denom (a' ! i ! i)
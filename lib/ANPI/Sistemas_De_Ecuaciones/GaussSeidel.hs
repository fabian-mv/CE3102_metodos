{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

module ANPI.Sistemas_De_Ecuaciones.GaussSeidel  (GaussSeidel (..))
where

import Numeric.LinearAlgebra

import ANPI.Sistemas_De_Ecuaciones.Base
import ANPI.Base

newtype GaussSeidel = GaussSeidel
  { x_k :: Vector R
  } deriving Show


instance Solucion Sistema GaussSeidel where
  error_k   sistema aprox = norm_2 $ a sistema #> x_k aprox - b sistema
  siguiente sistema aprox = GaussSeidel
    { x_k = -(sustitucionAdelante Sistema{a = ld, b = u * x_k }) +
    sustitucionAdelante Sistema{a = ld, b = b sistema } } where
      ld = (fst . ldu . a) sistema
      u  = (snd . ldu . a) sistema


sustitucionAdelante :: Sistema -> Vector
sustitucionAdelante sistema = fromList (foldl x [] [0..n a' - 1]) where
  x acc i = (: acc) . (/ denom (a' ! i ! i)) . sum . map (uncurry (*)) . zip (toList $ a' ! i) . reverse $ acc
  a'      = a sistema
  b'      = b sistema


ldu :: Matrix -> (Matrix, Matrix)
ldu matriz = (tri (>=), tri (<)) where
  tri filtro = build (size matriz) $ \i j ->
    if i `filtro` j then matriz ! i ! j else 0
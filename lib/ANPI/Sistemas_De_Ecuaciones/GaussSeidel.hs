{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, DeriveGeneric, DeriveAnyClass #-}

module ANPI.Sistemas_De_Ecuaciones.GaussSeidel  (GaussSeidel (..))
where

import GHC.Generics (Generic)

import Control.DeepSeq
import Numeric.LinearAlgebra

import ANPI.Sistemas_De_Ecuaciones.Base
import ANPI.Base

newtype GaussSeidel = GaussSeidel
  { x_k :: Vector R
  } deriving (Show, Generic, NFData)


instance Solucion Sistema GaussSeidel where
  error_k   sistema aprox = norm_2 $ a sistema #> x_k aprox - b sistema
  siguiente sistema aprox = GaussSeidel
    { x_k = sol ld (b sistema) - sol ld (u #> x_k aprox) } where
      (ld, u) = (ldu . a) sistema
      sol a b = sustitucionAdelante Sistema { a = a, b = b }


sustitucionAdelante :: Sistema -> Vector R
sustitucionAdelante sistema = fromList . reverse . foldl x [] $ [0..n a' - 1] where
  x acc i = (: acc) . (/ denom (a' ! i ! i)) . (b' ! i -) . sum . map (uncurry (*)) . zip (toList $ a' ! i) . reverse $ acc
  a'      = a sistema
  b'      = b sistema


ldu :: Matrix R -> (Matrix R, Matrix R)
ldu matriz = (tri (>=), tri (<)) where
  tri filtro = build (size matriz) $ \i j ->
    if i `filtro` j then matriz ! truncate i ! truncate j else 0

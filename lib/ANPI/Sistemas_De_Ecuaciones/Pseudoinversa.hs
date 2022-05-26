{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

module ANPI.Sistemas_De_Ecuaciones.Pseudoinversa (Pseudoinversa (..), x_0)
where

import Numeric.LinearAlgebra
import qualified Numeric.LinearAlgebra as LA

import ANPI.Sistemas_De_Ecuaciones.Base
import ANPI.Base
import Text.Read (Lexeme(Ident))

newtype Pseudoinversa = Pseudoinversa
  { x_k :: Matrix R
  } deriving Show

instance Solucion Sistema Pseudoinversa where
  error_k sistema aprox = norm_2 (x_kmas1' - x_k') / (denom . norm_2 $ x_kmas1') where
    (x_k', x_kmas1') = (x_k aprox, x_k . siguiente sistema $ aprox)

  siguiente sistema aprox = Pseudoinversa
    { x_k = x_k aprox LA.<> (_2i - a sistema LA.<> x_k aprox)
    } where _2i = (2 *) . ident . n . a $ sistema

x_0 :: Sistema -> Pseudoinversa
x_0 sistema = Pseudoinversa
  { x_k = (recip . denom . (^ 2) . norm_2 . a $ sistema) `scale` tr' (a sistema) }
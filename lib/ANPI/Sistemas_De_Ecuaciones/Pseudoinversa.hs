{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, DeriveGeneric, DeriveAnyClass #-}

module ANPI.Sistemas_De_Ecuaciones.Pseudoinversa (Pseudoinversa (..), x_0) where

import GHC.Generics (Generic)

import Control.DeepSeq
import Numeric.LinearAlgebra
import qualified Numeric.LinearAlgebra as LA

import ANPI.Sistemas_De_Ecuaciones.Base
import ANPI.Base
import Text.Read (Lexeme(Ident))

data Pseudoinversa = Pseudoinversa
  { pseudo_k :: Matrix R
  {- Por laziness esto no es ineficiente (no se calcula x_k en cada iteración),
   - ya que solamente se solicita el último x_k, los anteriores se ignoran y
   - por tanto nunca se computan del todo.
   -}
  , x_k      :: Vector R
  } deriving (Show, Generic, NFData)

instance Solucion Sistema Pseudoinversa where
  error_k sistema aprox = norm_2 (pseudo_kmas1' - pseudo_k') / (denom . norm_2) pseudo_kmas1'
    where (pseudo_k', pseudo_kmas1') = (pseudo_k aprox, pseudo_k . siguiente sistema $ aprox)

  siguiente sistema aprox = sol sistema $
    pseudo_k aprox LA.<> (_2i - a sistema LA.<> pseudo_k aprox)
    where _2i = (2 *) . ident . n . a $ sistema

x_0 :: Sistema -> Pseudoinversa
x_0 sistema = sol sistema $ (recip . denom . (^ 2) . norm_2 . a) sistema `scale` tr' (a sistema)

sol sistema pseudo_k = Pseudoinversa
  { pseudo_k = pseudo_k
  , x_k      = pseudo_k #> b sistema
  }

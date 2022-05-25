{-# LANGUAGE UnicodeSyntax #-}

import ANPI.Base
import ANPI.Ecuaciones_No_Lineales.Base
import ANPI.Sistemas_De_Ecuaciones.Base

import ANPI.Ecuaciones_No_Lineales.Secante

main :: IO ()
main = putStrLn . show $ solucion param x_0 where
  
  x_0 = Secante
    { x_km1 = 0
    , x_k   = 1
    }

  param = Param
    { objetivo  = \x -> exp (-x^2) - x
    , tol       = 1e-8
    , iterMax   = 1000
    }
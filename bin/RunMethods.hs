{-# LANGUAGE UnicodeSyntax #-}

import ANPI.Ecuaciones_No_Lineales.Secante
import ANPI.Base

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
{-# LANGUAGE UnicodeSyntax #-}

import ANPI.Ecuaciones_No_Lineales.FalsaPosicion
import ANPI.Base

main :: IO ()

main = putStrLn . show $ solucion param x_0 where
  
  x_0 = FalsaPosicion
    { a = 0.5
    , b = pi/4
    }

  param = Param
    { objetivo  = \x -> cos(x) - x
    , tol       = 10e-8
    , iterMax   = 1000
    }
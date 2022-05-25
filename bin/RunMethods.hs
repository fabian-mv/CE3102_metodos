import ANPI.Ecuaciones_No_Lineales.Biseccion
import ANPI.Base

main :: IO ()
main = putStrLn . show $ solucion param x_0 where
  
  x_0 = Biseccion
    { a = -1
    , b = 10
    }

  param = Param
    { objetivo  = \x -> x^2 - 3
    , tol       = 10e-8
    , iterMax   = 1000
    }
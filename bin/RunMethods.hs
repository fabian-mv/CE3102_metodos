import ANPI.Base
import ANPI.Ecuaciones_No_Lineales.Base
import ANPI.Sistemas_De_Ecuaciones.Base

import ANPI.Sistemas_De_Ecuaciones.GaussSeidel

import Numeric.LinearAlgebra

main :: IO ()
main = print $ solucion param x_0 where
  
  x_0 = GaussSeidel { x_k = 4 |> [0,0..] }

  param = Param
    { tol      = 1e-8
    , iterMax  = 1000
    , objetivo = Sistema
        { b = vector [6, 7, 7, 6]
        , a = (4><4)
            [ 5, 1, 0, 0
            , 1, 5, 1, 0
            , 0, 1, 5, 1
            , 0, 0, 1, 5]
        }
    }
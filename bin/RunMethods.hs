import Numeric.LinearAlgebra

import ANPI.Base
import ANPI.Ecuaciones_No_Lineales.Base
import ANPI.Sistemas_De_Ecuaciones.Base

import ANPI.Sistemas_De_Ecuaciones.Pseudoinversa

main :: IO ()
main = print $ solucion param (x_0 (objetivo param)) where

  param = Param
    { tol      = 1e-8
    , iterMax  = 1000
    , objetivo = Sistema
        { b = vector [4, -7, 13, -13]
        , a = (4><4)
            [ 1,  1, -1,   3
            , 0, -1, -1,  -5
            , 0,  0,  3,  13
            , 0,  0,  0, -13]
        }
    }
{-# LANGUAGE TupleSections #-}
 
import Control.Monad
import Text.Printf

import Criterion.Main
import Numeric.LinearAlgebra
import Text.Pretty.Simple (pPrint)

import ANPI.Base
import ANPI.Ecuaciones_No_Lineales.Base
import ANPI.Sistemas_De_Ecuaciones.Base
import ANPI.Sistemas_De_Ecuaciones.GaussSeidel

main :: IO ()
main = do
  forM_ pruebas $ \(metodo, pruebas) ->
    forM_ pruebas $ \((caso, param), aprox_0) -> do
      printf "[Método %s, caso %s]\n" metodo caso
      pPrint $ solucion param aprox_0
      putStrLn ""

  defaultMain
    [bgroup n
      [bench n' $ nf (solucion param) aprox_0
        | ((n', param), aprox_0) <- ps]
      | (n, ps) <- pruebas]

  where

  pruebas =
    [ ( "Gauss-Seidel"
      , map (, GaussSeidel { x_k = ceros 4 }) [sis4x4]
      )
    ]

  ceros = (|> [0, 0..])

  sis4x4 = ("4x4",) Param
    { tol      = 1e-8
    , iterMax  = 100
    , objetivo = Sistema
        { b = vector [6, 7, 7, 6]
        , a = (4><4)
            [ 5, 1, 0, 0
            , 1, 5, 1, 0
            , 0, 1, 5, 1
            , 0, 0, 1, 5]
        }
    }

{-# LANGUAGE Rank2Types, TupleSections #-}
 
import Control.Monad
import Text.Printf

import Control.DeepSeq
import Criterion.Main
import Numeric.LinearAlgebra
import Text.Pretty.Simple (pPrint)

import ANPI.Base
import ANPI.Ecuaciones_No_Lineales.Base
import ANPI.Sistemas_De_Ecuaciones.Base

import qualified ANPI.Ecuaciones_No_Lineales.Biseccion as Biseccion
import qualified ANPI.Ecuaciones_No_Lineales.NewtonRaphson as NewtonRaphson
import qualified ANPI.Ecuaciones_No_Lineales.Secante as Secante
import qualified ANPI.Ecuaciones_No_Lineales.FalsaPosicion as FalsaPosicion
import qualified ANPI.Sistemas_De_Ecuaciones.Jacobi as Jacobi
import qualified ANPI.Sistemas_De_Ecuaciones.GaussSeidel as GaussSeidel
import qualified ANPI.Sistemas_De_Ecuaciones.Pseudoinversa as Pseudoinversa

main :: IO ()
main = do
  sequence_ $ pruebas $ \metodo pruebas ->
    forM_ pruebas $ \((caso, param), aprox_0) -> do
      printf "[Método %s, caso %s]\n" metodo caso
      pPrint $ solucion param aprox_0
      putStrLn ""

  defaultMain $ pruebas $ \metodo ps -> bgroup metodo
    [bench n' $ nf (solucion param) aprox_0 | ((n', param), aprox_0) <- ps]

pruebas
  :: (forall s o. (Solucion o s, Show s, NFData s)
  => String -> [((String, Param o), s)] -> b)
  -> [b]

pruebas metodo =
  [ metodo "Bisección" $
      [(raiz3, Biseccion.Biseccion { Biseccion.a = -1, Biseccion.b = 10 })]

  , metodo "Newton-Raphson" $
      [(raiz3Derivable, NewtonRaphson.NewtonRaphson { NewtonRaphson.x_k = 1 })]

  , metodo "Secante" $
      [(raiz3, Secante.Secante { Secante.x_km1 = 1, Secante.x_k = 3 })]

  , metodo "falsa posición" $
      [(raiz3, FalsaPosicion.FalsaPosicion { FalsaPosicion.a = -1, FalsaPosicion.b = 10 })]

  , metodoSistema "Jacobi"
      (\s -> Jacobi.Jacobi { Jacobi.x_k = ceros s })
      [sis4x4]

  , metodoSistema "Gauss-Seidel"
      (\s -> GaussSeidel.GaussSeidel { GaussSeidel.x_k = ceros s }) 
      [sis4x4]

  , metodoSistema "pseudo-inversa"
      Pseudoinversa.x_0
      [sis4x4]
  ] where

  raiz3 = ("f(x) = x^2 - 3",) Param
    { objetivo  = \x -> x^2 - 3
    , tol       = 1e-8
    , iterMax   = 100
    }

  raiz3Derivable = ("f(x) = x^2 - 3, f'(x) = 2x",) Param
    { tol       = 1e-8
    , iterMax   = 100
    , objetivo  = Derivable
        { f  = \x -> x^2 - 3
        , f' = \x -> 2 * x
        }
    }

  metodoSistema nombre f =
    metodo nombre . map (\c@(_, p) -> (c, f $ objetivo p))

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

ceros :: Sistema -> Vector R
ceros = (|> [0, 0..]) . size . b


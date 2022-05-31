{-# LANGUAGE Rank2Types, TupleSections, BlockArguments #-}
 
import Control.Monad
import Text.Printf

import Control.DeepSeq
import Criterion.Main
import Numeric.LinearAlgebra
import Text.Pretty.Simple (pPrint)

import Funciones_ANPI.Base
import Funciones_ANPI.Ecuaciones_No_Lineales.Base
import Funciones_ANPI.Sistemas_De_Ecuaciones.Base
import Funciones_ANPI.Sistemas_De_Ecuaciones.FFI

import qualified Funciones_ANPI.Ecuaciones_No_Lineales.Biseccion as Biseccion
import qualified Funciones_ANPI.Ecuaciones_No_Lineales.NewtonRaphson as NewtonRaphson
import qualified Funciones_ANPI.Ecuaciones_No_Lineales.Secante as Secante
import qualified Funciones_ANPI.Ecuaciones_No_Lineales.Steffensen as Steffensen
import qualified Funciones_ANPI.Ecuaciones_No_Lineales.FalsaPosicion as FalsaPosicion

import qualified Funciones_ANPI.Sistemas_De_Ecuaciones.Jacobi as Jacobi
import qualified Funciones_ANPI.Sistemas_De_Ecuaciones.Thomas as Thomas
import qualified Funciones_ANPI.Sistemas_De_Ecuaciones.GradienteConjugado as GradienteConjugado
import qualified Funciones_ANPI.Sistemas_De_Ecuaciones.GaussSeidel as GaussSeidel
import qualified Funciones_ANPI.Sistemas_De_Ecuaciones.Pseudoinversa as Pseudoinversa

main :: IO ()
main = do
  pruebas (\_ -> return ()) $ \metodo pruebas ->
    forM_ pruebas $ \(caso, solucion, aprox_0) -> do
      printf "[Método %s, caso %s]\n" metodo caso
      salida <- solucion aprox_0
      pPrint salida
      putStrLn ""

  pruebas defaultMain $ \metodo ps -> pure $ bgroup metodo
    [bench n' $ nfIO (solucion aprox_0) | (n', solucion, aprox_0) <- ps]

pruebas
  :: ([a] -> IO b)
  -> (forall s o. (Show s, NFData s) => String -> [(String, o -> IO s, o)] -> IO a)
  -> IO b

pruebas operacion metodo = do
  Steffensen.withForeign (objetivo $ snd raiz3) $ \raiz3Cpp ->
    withCSistema sistemaPrueba $ \sis4x4Cpp -> do
      problemas <- sequence
        [ metodo "Bisección" $
            [lineal raiz3 Biseccion.Biseccion { Biseccion.a = -1, Biseccion.b = 10 }]

        , metodo "Newton-Raphson" $
            [lineal raiz3Derivable NewtonRaphson.NewtonRaphson { NewtonRaphson.x_k = 1 }]

        , metodo "Secante" $
            [lineal raiz3 Secante.Secante { Secante.x_km1 = 1, Secante.x_k = 3 }]

        , metodo "falsa posición" $
            [lineal raiz3 FalsaPosicion.FalsaPosicion { FalsaPosicion.a = -1, FalsaPosicion.b = 10 }]

        , metodo "Steffensen" $
            [lineal (raiz3Con raiz3Cpp) Steffensen.Steffensen { Steffensen.x_k = 1.2345 }]

        , metodoSistema "Jacobi"
            (\s -> Jacobi.Jacobi { Jacobi.x_k = ceros s })
            [sis4x4]

        , metodoSistema "Gauss-Seidel"
            (\s -> GaussSeidel.GaussSeidel { GaussSeidel.x_k = ceros s }) 
            [sis4x4]

        , metodo "Thomas" $
            [("4x4", Thomas.solucion, sis4x4Cpp)]

        , metodoSistema "pseudo-inversa"
            Pseudoinversa.x_0
            [sis4x4]

        , metodo "gradiente conjugado" $
            [("4x4", GradienteConjugado.solucion, sis4x4Cpp)]
        ]

      operacion problemas

  where

  raiz3 = raiz3Con $ \x -> x^2 - 3

  raiz3Con objetivo = ("f(x) = x^2 - 3",) Param
    { objetivo  = objetivo
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

  lineal (nombre, param) = (nombre, pure . solucion param,)

  metodoSistema nombre f =
    metodo nombre . map (\(n, p) -> (n, pure . solucion p, f $ objetivo p))

  sis4x4 = ("4x4",) Param
    { tol      = 1e-8
    , iterMax  = 100
    , objetivo = sistemaPrueba
    }

  sistemaPrueba = Sistema
    { b = vector [6, 7, 7, 6]
    , a = (4><4)
        [ 5, 1, 0, 0
        , 1, 5, 1, 0
        , 0, 1, 5, 1
        , 0, 0, 1, 5]
    }

  ceros :: Sistema -> Vector R
  ceros = (|> [0, 0..]) . size . b


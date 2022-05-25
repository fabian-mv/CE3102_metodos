module ANPI.Matriz (Iteracion (..), Param (..), a) where

import Numeric.LinearAlgebra

{-
  Crea el ADT con los parámetros.
-}
data Param = Param
  -- Haskell no permite llamarle "A" mayúscula
  { b       :: Vector R
  , m       :: Int
  , p       :: Vector R
  , q       :: Vector R
  , tol     :: R
  , iterMax :: Int
  }

a param = tridiagonal (p param) (q param) (m param)

{-
  Crea el ADT que representa cada iteración.
-}
data Iteracion = Iteracion
  { k   :: Int
  , x_k :: Vector R
  , err :: R
  } deriving Show

{-
  Describe la matriz tridiagonal.
-}
tridiagonal :: Vector R -> Vector R -> Int -> Matrix R
tridiagonal p q m = verificar matriz where
  matriz = build (m, m) (\i j -> entrada (truncate i) (truncate j))

  verificar
    | m < 3           = error "m debe ser entero positivo >= 3"
    | size p /= m - 1 = error "p debe ser un vector de m - 1 elementos"
    | size q /= m - 1 = error "q debe ser un vector de m - 1 elementos"
    | otherwise       = id

  entrada i j
    | i == j     = 2 * (p `sub` (i - 1) + q `sub` i)
    | i == j + 1 = p `sub` j
    | i == j - 1 = q `sub` i
    | otherwise  = 0

  sub v i
    | i >= 0 && i < m - 1 = v ! i
    | otherwise           = 0

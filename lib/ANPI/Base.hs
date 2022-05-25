module ANPI.Base (Iteracion (..), Param (..), Solucion (..), solucion) where

data Param o = Param
  { objetivo  :: o
  , tol       :: Double
  , iterMax   :: Int
  }


type Criterio = Double -> Double


data Iteracion s = Iteracion
  { k        :: Int
  , aprox_k  :: s
  , err      :: Double
  } deriving Show


class Solucion o s where
  error :: o -> s -> Double
  siguiente :: o -> s -> s


solucion :: Solucion o s => Param o -> s -> Iteracion s
solucion param x_0 = head . dropWhile (not . parar) . iterate sucesor $
  Iteracion { k = 0, x_k = x_0, err = error param x_0 } where

  parar iter = k iter >= iterMax param || error param (aprox_k iter) < tol param
  
  sucesor iter =
    let aprox_km1 = siguiente param iter
    in  Iteracion
        { k       = k iter + 1
        , aprox_k = aprox_km1
        , err     = error param aprox_km1
        }